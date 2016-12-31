var emptyNode = () => [];

var insert = (values, oldNode) =>
{
  if (values.length === 0)
  {
    return emptyNode();
  }
  else if (oldNode.length === 0)
  {
    var out =
    [
      {
        value:  values[0],
        branch: insert( values.slice(1), emptyNode() )
      }
    ];
    return out;
  }
  else
  {
    var out = oldNode.filter(e => e.value !== values[0]);
    if (out.length === oldNode.length)
    { // means current value is not there
      out.push({
        value: values[0],
        branch: insert( values.slice(1), emptyNode() )
      });
    }
    else
    {
      var existingBranch = oldNode.find( e => e.value === values[0]).branch;
      out.push(
        {
          value:  values[0],
          branch: insert( values.slice(1), existingBranch )
        }
      );
    }
    return out;
  }
}

var newTree = (values) => insert(values, emptyNode());

var flatTree = (tree) =>
{
  if (typeof acc === 'undefined')
  { // first call
    acc = [];
  }
  var root = tree.map( e => e.value );
  var branch = tree.map( e => flatTree(e.branch) ).reduce( (acc, e) => acc.concat( e ), []);
  var out = root.concat( branch );
  return out;
}

// test
var t = newTree('abcd');
console.log( flatTree(t) );
var t2 = insert('aber', t);
console.log( flatTree(t2) );

debugger;