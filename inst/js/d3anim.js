var w = 500,
    h = 500,
    pad = 20,
    left_pad = 50,
    initialised = 0,
    aps,
    fps,
    palette,
    colourmap = {},
    density = 1;


    // colorbrewer scales @ https://bl.ocks.org/mbostock/5577023
    // and http://colorbrewer2.org/

Shiny.addCustomMessageHandler("parameters", function(message) {aps = Number(message[0]), fps = Number(message[1])});

var duration = 1000 / 1;

var svg = d3.select("#d3_output_2")
    .append("svg")
    .attr("width", w)
    .attr("height", h);

var x = d3.scaleLinear().domain([-1, 1]).range([left_pad, w - pad]),
    y = d3.scaleLinear().domain([-1, 1]).range([pad, h - pad * 2]);

var xCenter = (w - left_pad)/2;
var yCenter = ((h-pad*2) - pad)/2;

var xAxis = d3.axisBottom(x)
    .ticks(10);

var yAxis = d3.axisLeft(y)
    .ticks(10);

svg.append("g")
    .attr("class", "axis")
    .attr("transform", "translate(0, " + (h - pad) + ")")
    .selectAll("text").remove()
    .call(xAxis);

svg.append("g")
    .attr("class", "axis")
    .attr("transform", "translate(" + (left_pad - pad) + ", 0)")
    .selectAll("text").remove()
    .call(yAxis);

svg.append("text")
    .attr("class", "loading")
    .text("Loading ...")
    .attr("x", function() {
        return w / 2;
    })
    .attr("y", function() {
        return h / 2 - 5;
    });

Shiny.addCustomMessageHandler("info",
  function (message) {
    document.getElementById('info').innerHTML = message;
  }
)


Shiny.addCustomMessageHandler("debug",
  function (message){
    document.getElementById('d3_output').innerHTML = message;
  })

Shiny.addCustomMessageHandler("newcolours",
function(message) {

  svg.selectAll(".legend").remove();

  draw_legend = function(message) {
          var legend = svg.selectAll(".legend")
      .data(message)
    .enter().append("g")
      .attr("class", "legend")
      .attr("transform", function(d, i) { return "translate(0," + i * 20 + ")"; });

       // draw legend colored rectangles
  legend.append("rect")
      .attr("x", w - 18)
      .attr("width", 18)
      .attr("height", 18)
      .style("fill", function(d) {
        return colourmap[d]
      });


      legend.append("text")
      .attr("x", w - 24)
      .attr("y", 9)
      .attr("dy", ".35em")
      .style("text-anchor", "end")
      .text(function(d) { return d;});

        }

  l = message.length
  palette = d3.scaleOrdinal(d3.schemeDark2);

  for (i = 0; i < l; i++) {
    colourmap[message[i]] = palette(i); // for discrete palettes
    // colourmap[message[i]] = palette((i+1)/(l+1)); // for continuous palettes
    }

    draw_legend(message);

}
)


Shiny.addCustomMessageHandler("data",
    function(message) {


        // document.getElementById('d3_output').innerHTML = message;

        svg.select(".loading").remove();

        draw_scatterplot = function(message) {
          svg.selectAll("circle")
            .data(message.d)
            .enter()
            .append("circle")
            .attr("class", "circle")
            .attr("cx", function(d) {
                return x(d.x);
            })
            .attr("cy", function(d) {
                return y(d.y);
            })
            .attr("r", 2)
            .attr("fill", function(d) {
              return colourmap[d.c]
            })
            .append("svg:title")
            .text(function(d) {return d.pL})

        svg.selectAll(".line")
        .data(message.a)
        .enter()
        .append("line")
        .attr("x1",xCenter)
        .attr("y1",yCenter)
        .attr("x2",function(d) {return x(d.x)})
        .attr("y2", function(d) {return y(d.y)})
        .attr("stroke-width", 2)
        .attr("stroke","black");
        svg.selectAll(".text1")
        .data(message.a)
        .enter()
        .append("text")
        .attr("class","text1")
        .attr("x", function(d) {return x(d.x)})
        .attr("y", function(d) {return y(d.y)})
        .text(function(d) {return d.n})


        }




        if (initialised === 0) {
           draw_scatterplot(message);
        } else  {


          svg.selectAll("path").remove();
          svg.selectAll("circle").remove();
          svg.selectAll("line").remove();
          svg.selectAll(".text1").remove();
          draw_scatterplot(message);

            svg.selectAll("path").remove();
        }

        initialised = 1;


    });
