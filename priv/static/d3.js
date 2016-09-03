var n = 40,
    random = d3.randomUniform(0, 1),
    data = d3.range(n).map(random);
var svg = d3.select("svg"),
    margin = {top: 20, right: 0, bottom: 5, left: 30},
    width = +svg.attr("width") - margin.left - margin.right,
    height = +svg.attr("height") - margin.top - margin.bottom,
    g = svg.append("g").attr("transform", "translate(" + margin.left + "," + margin.top + ")");
var x = d3.scaleLinear()
    .domain([0, n - 1])
    .range([0, width]);
var y = d3.scaleLinear()
    .domain([-0.01, 1],0.2)
    .range([height,0]);
var line = d3.line()
    .x(function(d, i) { return x(i); })
    .y(function(d, i) { return y(d); });
g.append("defs").append("clipPath")
    .attr("id", "clip")
  .append("rect")
    .attr("width", width)
    .attr("height", height);
g.append("g")
    .attr("class", "axis axis--x")
    .attr("transform", "translate(0," + y(0) + ")")
    .call(d3.axisBottom(x));
g.append("g")
    .attr("class", "axis axis--y")
    .call(d3.axisLeft(y));
g.append("g")
    .attr("clip-path", "url(#clip)")
  .append("path")
    .datum(data)
    .attr("class", "line")
  .transition()
    .duration(600)
    .ease(d3.easeLinear)
    .on("start", tick);
function tick() {
  // Push a new data point onto the back.
  data.push(random());
  // Redraw the line.
  d3.select(this)
      .attr("d", line)
      .attr("transform", null);
  // Slide it to the left.
  d3.active(this)
      .attr("transform", "translate(" + x(-1) + ",0)")
    .transition()
      .on("start", tick);
  // Pop the old data point off the front.
  data.shift();
}
