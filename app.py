import numpy as np
import pandas as pd
from shiny import reactive, req
from shiny.express import input, render, ui

# Page configuration and CDN scripts
ui.page_opts(title="Shiny - Interactive Scatter & Regression", fillable=False)

# Include Tailwind CSS, D3, and Observable Plot via HTML head tags
ui.head_content(
    ui.tags.script(src="https://cdn.tailwindcss.com"),
    ui.tags.script(src="https://cdn.jsdelivr.net/npm/d3@7"),
    ui.tags.script(src="https://cdn.jsdelivr.net/npm/@observablehq/plot@0.6"),
)

# Custom JavaScript function to render Observable Plot inside Shiny
CUSTOM_PLOT_JS = """
Shiny.addCustomMessageHandler('render-observable-plot', function(payload) {
    const container = document.getElementById('plot-container');
    if (!container) return;
    
    container.innerHTML = ''; // Clear previous plot
    
    const dataset = payload.data;
    const targetR = payload.r;
    const N = payload.n;
    const isPositive = targetR >= 0;

    const chart = Plot.plot({
        width: 760,
        height: 460,
        style: {
            background: "transparent",
            color: "#94a3b8"
        },
        grid: true,
        x: {
            label: "Variable X (Standard Normal)",
            domain: [-3.5, 3.5]
        },
        y: {
            label: "Variable Y (Standard Normal)",
            domain: [-3.5, 3.5]
        },
        marks: [
            // Zero reference axes
            Plot.ruleX([0], { stroke: "#475569", strokeDasharray: "4,4" }),
            Plot.ruleY([0], { stroke: "#475569", strokeDasharray: "4,4" }),
            
            // Scatter points
            Plot.dot(dataset, {
                x: "x",
                y: "y",
                fill: "#818cf8",
                fillOpacity: 0.6,
                r: 4.5,
                stroke: "#312e81",
                strokeWidth: 1
            }),

            // Linear Regression Trend Line
            Plot.linearRegressionY(dataset, {
                x: "x",
                y: "y",
                stroke: isPositive ? "#38bdf8" : "#f43f5e",
                strokeWidth: 3
            }),

            // Overlay Text
            Plot.tip([`N = ${N} | Target r = ${targetR.toFixed(2)}`], {
                x: -3.2,
                y: 3.2,
                anchor: "top-left",
                fill: "#1e293b",
                stroke: "#475569"
            })
        ]
    });

    container.appendChild(chart);
});
"""

ui.head_content(ui.tags.script(CUSTOM_PLOT_JS))

# Main UI Layout using Tailwind CSS classes
with ui.div(class_="bg-slate-900 text-slate-100 min-h-screen font-sans p-6"):
    with ui.div(class_="max-w-4xl mx-auto space-y-6"):

        # Header
        with ui.tags.header(class_="border-b border-slate-800 pb-4"):
            ui.tags.h1(
                "Interactive Bivariate Normal Scatter Plot",
                class_="text-2xl font-bold text-white",
            )
            ui.tags.p(
                "Generated with Shiny for Python & Observable Plot. Adjust correlation strength and direction to update the distribution and regression line in real time.",
                class_="text-slate-400 text-sm mt-1",
            )

        # Controls Panel
        with ui.div(
            class_="bg-slate-800/60 rounded-xl border border-slate-800 p-5 space-y-5"
        ):
            with ui.div(
                class_="grid grid-cols-1 md:grid-cols-2 gap-6 items-center"
            ):

                # Direction Toggle
                with ui.div(class_="space-y-2"):
                    ui.tags.label(
                        "Relationship Direction",
                        class_="text-xs font-semibold text-slate-400 uppercase tracking-wider block",
                    )
                    ui.input_radio_buttons(
                        "direction",
                        None,
                        choices={"pos": "Positive (+)", "neg": "Negative (−)"},
                        selected="pos",
                        inline=True,
                    )

                # Strength Slider
                with ui.div(class_="space-y-2"):
                    with ui.div(class_="flex justify-between items-center"):
                        ui.tags.label(
                            "Strength of Association",
                            class_="text-xs font-semibold text-slate-400 uppercase tracking-wider",
                        )

                        @render.text
                        def correlation_label():
                            sign = 1 if input.direction() == "pos" else -1
                            r = sign * input.magnitude()
                            prefix = "+" if r >= 0 else ""
                            return f"r = {prefix}{r:.2f}"

                    ui.input_slider(
                        "magnitude",
                        None,
                        min=0.0,
                        max=0.99,
                        value=0.70,
                        step=0.01,
                    )

            # Resample Control
            with ui.div(
                class_="pt-2 border-t border-slate-700/50 flex justify-between items-center"
            ):
                ui.tags.span(
                    "Sample Size: N = 250", class_="text-xs text-slate-500"
                )
                ui.input_action_button(
                    "resample",
                    "Resample Data",
                    class_="text-xs text-indigo-400 hover:text-indigo-300 font-medium underline underline-offset-4 bg-transparent border-0 p-0",
                )

        # Plot Output Container
        with ui.div(
            class_="bg-slate-800/40 rounded-xl border border-slate-800 p-4 flex justify-center items-center"
        ):
            ui.tags.div(id="plot-container", class_="w-full overflow-x-auto")


# Reactive Data Generator
@reactive.calc
def current_data():
    input.resample()  # Re-run when "Resample Data" button is clicked

    sign = 1 if input.direction() == "pos" else -1
    r = sign * input.magnitude()
    n = 250

    abs_r = min(abs(r), 0.999)
    noise_weight = np.sqrt(1 - abs_r**2)

    x = np.random.normal(0, 1, n)
    z = np.random.normal(0, 1, n)
    y = r * x + noise_weight * z

    df = pd.DataFrame({"x": x, "y": y})
    return {"data": df.to_dict(orient="records"), "r": r, "n": n}


# Effect to send reactive updates directly to Observable Plot JS
@reactive.effect
async def _update_observable_plot():
    payload = current_data()

    # Send custom message directly to frontend handler
    from shiny.session import get_current_session

    session = get_current_session()
    if session:
        await session.send_custom_message("render-observable-plot", payload)
