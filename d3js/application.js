// select always returns an array object (similar to how jQuery is an arrayish object)
var data = ['hello', 'there', 'test'];

var foos = d3.select('body').selectAll('p').data(data).text(String);

// creates __data__ attribute in each DOM node object and puts an array element
// in it.
//

foos.enter().append('p').text(String);

// remove nodes on exit

foos.exit().remove();

// here #foo must already exist in the DOM
var foo = d3.select('#foo');
var div = foo.append('div');
div.html('hi there');

// chart

var data = [4, 8, 15, 16, 23, 42];

d3.select('.chart')

  // define the selection to which we will join the data
  // (these elements do not have to exist yet)
  .selectAll('div')

  // then join the data to the selection
  // this returns an "update selection"
  .data(data)

  // calling .enter() on an update selection gives you its "enter selection"
  // which represents elements of the data array which don't already have a node
  .enter()

  // create a node for each element
  // this returns just the set of "enter nodes" (
  .append('div')

  // then for each enter node set style and content
  .style('width', function (d) {
    return d * 10 + 'px';
  })
  .text(function (d) {
    return d;
  });
