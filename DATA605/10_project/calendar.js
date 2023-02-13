// !preview r2d3 data = read.csv("dji-latest.csv"), d3_version = 4, container = "div", options = list(start = 2006, end = 2011)

// Based on https://bl.ocks.org/mbostock/4063318

var height = height / (options.end - options.start),
    cellSize = height / 10; // set cellSize to 1/10 of the height

var formatPercent = d3.format(".1%");

var color = d3.scaleQuantize()
    .domain([1, 57110])
    .range(["#f7fcf5", "#e5f5e0", "#c7e9c0", "#a1d99b", "#74c476", "#41ab5d", "#238b45", "#006d2c", "#00441b", "#003c17", "#00280f"]);


var svg = div
    .style("line-height", "0")
    .selectAll("svg")
    .data(d3.range(options.start, options.end))
    .enter().append("svg")
    .attr("width", width)
    .attr("height", height)
    .append("g")
    .attr("transform", "translate(" + cellSize * 3.5 + "," + (height - cellSize * 7 - 1) + ")");

svg.append("text")
    .attr("transform", "translate(-" + (6 * height / 60) + "," + cellSize * 3.5 + ")rotate(-90)")
    .attr("font-family", "sans-serif")
    .attr("font-size", 2 + 8 * height / 60)
    .attr("text-anchor", "middle")
    .text(function(d) { return d; });

var rect = svg.append("g")
    .attr("fill", "none")
    .attr("stroke", "#ccc")
    .attr("stroke-width", "0.25")
    .selectAll("rect")
    .data(function(d) { return d3.timeDays(new Date(d, 0, 1), new Date(d + 1, 0, 1)); })
    .enter().append("rect")
    .attr("width", cellSize)
    .attr("height", cellSize)
    .attr("x", function(d) { return d3.timeWeek.count(d3.timeYear(d), d) * cellSize; })
    .attr("y", function(d) { return d.getDay() * cellSize; })
    .datum(d3.timeFormat("%Y-%m-%d"));

svg.append("g")
    .attr("fill", "none")
    .attr("stroke", "#000")
    .attr("stroke-width", "0.25")
    .selectAll("path")
    .data(function(d) { return d3.timeMonths(new Date(d, 0, 1), new Date(d + 1, 0, 1)); })
    .enter().append("path")
    .attr("d", pathMonth);

r2d3.onRender(function(csv, div, width, height, options) {
    var data = d3.nest()
        .key(function(d) { return d.Date; })
        .rollup(function(d) { return d[0].Duration; })
        .object(csv);

    rect.filter(function(d) { return d in data; })
        .attr("fill", function(d) { return color(data[d]); })
        .append("title")
        .text(function(d) { return d + ": " + formatPercent(data[d]); });
});

function pathMonth(t0) {
    var t1 = new Date(t0.getFullYear(), t0.getMonth() + 1, 0),
        d0 = t0.getDay(), w0 = d3.timeWeek.count(d3.timeYear(t0), t0),
        d1 = t1.getDay(), w1 = d3.timeWeek.count(d3.timeYear(t1), t1);
    return "M" + (w0 + 1) * cellSize + "," + d0 * cellSize
        + "H" + w0 * cellSize + "V" + 7 * cellSize
        + "H" + w1 * cellSize + "V" + (d1 + 1) * cellSize
        + "H" + (w1 + 1) * cellSize + "V" + 0
        + "H" + (w0 + 1) * cellSize + "Z";
}

