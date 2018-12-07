(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}




var _List_Nil = { $: 0 };
var _List_Nil_UNUSED = { $: '[]' };

function _List_Cons(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons_UNUSED(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === elm$core$Basics$EQ ? 0 : ord === elm$core$Basics$LT ? -1 : 1;
	}));
});



// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	/**_UNUSED/
	if (x.$ === 'Set_elm_builtin')
	{
		x = elm$core$Set$toList(x);
		y = elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = elm$core$Dict$toList(x);
		y = elm$core$Dict$toList(y);
	}
	//*/

	/**/
	if (x.$ < 0)
	{
		x = elm$core$Dict$toList(x);
		y = elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**_UNUSED/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**/
	if (!x.$)
	//*/
	/**_UNUSED/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? elm$core$Basics$LT : n ? elm$core$Basics$GT : elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0 = 0;
var _Utils_Tuple0_UNUSED = { $: '#0' };

function _Utils_Tuple2(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2_UNUSED(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3_UNUSED(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr(c) { return c; }
function _Utils_chr_UNUSED(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log = F2(function(tag, value)
{
	return value;
});

var _Debug_log_UNUSED = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString(value)
{
	return '<internals>';
}

function _Debug_toString_UNUSED(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[94m' + string + '\x1b[0m' : string;
}



// CRASH


function _Debug_crash(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash_UNUSED(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.aH.O === region.a0.O)
	{
		return 'on line ' + region.aH.O;
	}
	return 'on lines ' + region.aH.O + ' through ' + region.a0.O;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800)
			+
			String.fromCharCode(code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return word
		? elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? elm$core$Maybe$Nothing
		: elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? elm$core$Maybe$Just(n) : elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




/**_UNUSED/
function _Json_errorToString(error)
{
	return elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

var _Json_decodeInt = { $: 2 };
var _Json_decodeBool = { $: 3 };
var _Json_decodeFloat = { $: 4 };
var _Json_decodeValue = { $: 5 };
var _Json_decodeString = { $: 6 };

function _Json_decodeList(decoder) { return { $: 7, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 8, b: decoder }; }

function _Json_decodeNull(value) { return { $: 9, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 10,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 11,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 12,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 13,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 14,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 15,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 3:
			return (typeof value === 'boolean')
				? elm$core$Result$Ok(value)
				: _Json_expecting('a BOOL', value);

		case 2:
			if (typeof value !== 'number') {
				return _Json_expecting('an INT', value);
			}

			if (-2147483647 < value && value < 2147483647 && (value | 0) === value) {
				return elm$core$Result$Ok(value);
			}

			if (isFinite(value) && !(value % 1)) {
				return elm$core$Result$Ok(value);
			}

			return _Json_expecting('an INT', value);

		case 4:
			return (typeof value === 'number')
				? elm$core$Result$Ok(value)
				: _Json_expecting('a FLOAT', value);

		case 6:
			return (typeof value === 'string')
				? elm$core$Result$Ok(value)
				: (value instanceof String)
					? elm$core$Result$Ok(value + '')
					: _Json_expecting('a STRING', value);

		case 9:
			return (value === null)
				? elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 5:
			return elm$core$Result$Ok(_Json_wrap(value));

		case 7:
			if (!Array.isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 8:
			if (!Array.isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 10:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return (elm$core$Result$isOk(result)) ? result : elm$core$Result$Err(A2(elm$json$Json$Decode$Field, field, result.a));

		case 11:
			var index = decoder.e;
			if (!Array.isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return (elm$core$Result$isOk(result)) ? result : elm$core$Result$Err(A2(elm$json$Json$Decode$Index, index, result.a));

		case 12:
			if (typeof value !== 'object' || value === null || Array.isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!elm$core$Result$isOk(result))
					{
						return elm$core$Result$Err(A2(elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return elm$core$Result$Ok(elm$core$List$reverse(keyValuePairs));

		case 13:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return elm$core$Result$Ok(answer);

		case 14:
			var result = _Json_runHelp(decoder.b, value);
			return (!elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 15:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if (elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return elm$core$Result$Err(elm$json$Json$Decode$OneOf(elm$core$List$reverse(errors)));

		case 1:
			return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!elm$core$Result$isOk(result))
		{
			return elm$core$Result$Err(A2(elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return elm$core$Result$Ok(toElmValue(array));
}

function _Json_toElmArray(array)
{
	return A2(elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 3:
		case 2:
		case 4:
		case 6:
		case 5:
			return true;

		case 9:
			return x.c === y.c;

		case 7:
		case 8:
		case 12:
			return _Json_equality(x.b, y.b);

		case 10:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 11:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 13:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 14:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 15:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap_UNUSED(value) { return { $: 0, a: value }; }
function _Json_unwrap_UNUSED(value) { return value.a; }

function _Json_wrap(value) { return value; }
function _Json_unwrap(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.be,
		impl.bI,
		impl.bE,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	elm$core$Result$isOk(result) || _Debug_crash(2 /**_UNUSED/, _Json_errorToString(result.a) /**/);
	var managers = {};
	result = init(result.a);
	var model = result.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		result = A2(update, msg, model);
		stepper(model = result.a, viewMetadata);
		_Platform_dispatchEffects(managers, result.b, subscriptions(model));
	}

	_Platform_dispatchEffects(managers, result.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				p: bag.n,
				q: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.q)
		{
			x = temp.p(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		r: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].r;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		r: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].r;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**/
	var node = args['node'];
	//*/
	/**_UNUSED/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2(elm$json$Json$Decode$map, func, handler.a)
				:
			A3(elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		cg: func(record.cg),
		aJ: record.aJ,
		aE: record.aE
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.cg;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.aJ;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.aE) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.be,
		impl.bI,
		impl.bE,
		function(sendToApp, initialModel) {
			var view = impl.bP;
			/**/
			var domNode = args['node'];
			//*/
			/**_UNUSED/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.be,
		impl.bI,
		impl.bE,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.P && impl.P(sendToApp)
			var view = impl.bP;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.f);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.bG) && (_VirtualDom_doc.title = title = doc.bG);
			});
		}
	);
});



// ANIMATION


var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.bl;
	var onUrlRequest = impl.bm;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		P: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.download)
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.bt === next.bt
							&& curr.bc === next.bc
							&& curr.bp.a === next.bp.a
						)
							? elm$browser$Browser$Internal(next)
							: elm$browser$Browser$External(href)
					));
				}
			});
		},
		be: function(flags)
		{
			return A3(impl.be, flags, _Browser_getUrl(), key);
		},
		bP: impl.bP,
		bI: impl.bI,
		bE: impl.bE
	});
}

function _Browser_getUrl()
{
	return elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return elm$core$Result$isOk(result) ? elm$core$Maybe$Just(result.a) : elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { cb: 'hidden', N: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { cb: 'mozHidden', N: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { cb: 'msHidden', N: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { cb: 'webkitHidden', N: 'webkitvisibilitychange' }
		: { cb: 'hidden', N: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail(elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		by: _Browser_getScene(),
		bQ: {
			d: _Browser_window.pageXOffset,
			e: _Browser_window.pageYOffset,
			L: _Browser_doc.documentElement.clientWidth,
			B: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		L: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		B: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			by: {
				L: node.scrollWidth,
				B: node.scrollHeight
			},
			bQ: {
				d: node.scrollLeft,
				e: node.scrollTop,
				L: node.clientWidth,
				B: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			by: _Browser_getScene(),
			bQ: {
				d: x,
				e: y,
				L: _Browser_doc.documentElement.clientWidth,
				B: _Browser_doc.documentElement.clientHeight
			},
			b1: {
				d: x + rect.left,
				e: y + rect.top,
				L: rect.width,
				B: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}



// SEND REQUEST

var _Http_toTask = F2(function(request, maybeProgress)
{
	return _Scheduler_binding(function(callback)
	{
		var xhr = new XMLHttpRequest();

		_Http_configureProgress(xhr, maybeProgress);

		xhr.addEventListener('error', function() {
			callback(_Scheduler_fail(elm$http$Http$NetworkError));
		});
		xhr.addEventListener('timeout', function() {
			callback(_Scheduler_fail(elm$http$Http$Timeout));
		});
		xhr.addEventListener('load', function() {
			callback(_Http_handleResponse(xhr, request.j.a));
		});

		try
		{
			xhr.open(request.l, request.n, true);
		}
		catch (e)
		{
			return callback(_Scheduler_fail(elm$http$Http$BadUrl(request.n)));
		}

		_Http_configureRequest(xhr, request);

		var body = request.f;
		xhr.send(elm$http$Http$Internal$isStringBody(body)
			? (xhr.setRequestHeader('Content-Type', body.a), body.b)
			: body.a
		);

		return function() { xhr.abort(); };
	});
});

function _Http_configureProgress(xhr, maybeProgress)
{
	if (!elm$core$Maybe$isJust(maybeProgress))
	{
		return;
	}

	xhr.addEventListener('progress', function(event) {
		if (!event.lengthComputable)
		{
			return;
		}
		_Scheduler_rawSpawn(maybeProgress.a({
			bX: event.loaded,
			bY: event.total
		}));
	});
}

function _Http_configureRequest(xhr, request)
{
	for (var headers = request.k; headers.b; headers = headers.b) // WHILE_CONS
	{
		xhr.setRequestHeader(headers.a.a, headers.a.b);
	}

	xhr.responseType = request.j.b;
	xhr.withCredentials = request.o;

	elm$core$Maybe$isJust(request.m) && (xhr.timeout = request.m.a);
}


// RESPONSES

function _Http_handleResponse(xhr, responseToResult)
{
	var response = _Http_toResponse(xhr);

	if (xhr.status < 200 || 300 <= xhr.status)
	{
		response.body = xhr.responseText;
		return _Scheduler_fail(elm$http$Http$BadStatus(response));
	}

	var result = responseToResult(response);

	if (elm$core$Result$isOk(result))
	{
		return _Scheduler_succeed(result.a);
	}
	else
	{
		response.body = xhr.responseText;
		return _Scheduler_fail(A2(elm$http$Http$BadPayload, result.a, response));
	}
}

function _Http_toResponse(xhr)
{
	return {
		n: xhr.responseURL,
		cm: { Z: xhr.status, cg: xhr.statusText },
		k: _Http_parseHeaders(xhr.getAllResponseHeaders()),
		f: xhr.response
	};
}

function _Http_parseHeaders(rawHeaders)
{
	var headers = elm$core$Dict$empty;

	if (!rawHeaders)
	{
		return headers;
	}

	var headerPairs = rawHeaders.split('\u000d\u000a');
	for (var i = headerPairs.length; i--; )
	{
		var headerPair = headerPairs[i];
		var index = headerPair.indexOf('\u003a\u0020');
		if (index > 0)
		{
			var key = headerPair.substring(0, index);
			var value = headerPair.substring(index + 2);

			headers = A3(elm$core$Dict$update, key, function(oldValue) {
				return elm$core$Maybe$Just(elm$core$Maybe$isJust(oldValue)
					? value + ', ' + oldValue.a
					: value
				);
			}, headers);
		}
	}

	return headers;
}


// EXPECTORS

function _Http_expectStringResponse(responseToResult)
{
	return {
		$: 0,
		b: 'text',
		a: responseToResult
	};
}

var _Http_mapExpect = F2(function(func, expect)
{
	return {
		$: 0,
		b: expect.b,
		a: function(response) {
			var convertedResponse = expect.a(response);
			return A2(elm$core$Result$map, func, convertedResponse);
		}
	};
});


// BODY

function _Http_multipart(parts)
{


	for (var formData = new FormData(); parts.b; parts = parts.b) // WHILE_CONS
	{
		var part = parts.a;
		formData.append(part.a, part.b);
	}

	return elm$http$Http$Internal$FormDataBody(formData);
}



var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});


function _Url_percentEncode(string)
{
	return encodeURIComponent(string);
}

function _Url_percentDecode(string)
{
	try
	{
		return elm$core$Maybe$Just(decodeURIComponent(string));
	}
	catch (e)
	{
		return elm$core$Maybe$Nothing;
	}
}

// CREATE

var _Regex_never = /.^/;

var _Regex_fromStringWith = F2(function(options, string)
{
	var flags = 'g';
	if (options.bj) { flags += 'm'; }
	if (options.aU) { flags += 'i'; }

	try
	{
		return elm$core$Maybe$Just(new RegExp(string, flags));
	}
	catch(error)
	{
		return elm$core$Maybe$Nothing;
	}
});


// USE

var _Regex_contains = F2(function(re, string)
{
	return string.match(re) !== null;
});


var _Regex_findAtMost = F3(function(n, re, str)
{
	var out = [];
	var number = 0;
	var string = str;
	var lastIndex = re.lastIndex;
	var prevLastIndex = -1;
	var result;
	while (number++ < n && (result = re.exec(string)))
	{
		if (prevLastIndex == re.lastIndex) break;
		var i = result.length - 1;
		var subs = new Array(i);
		while (i > 0)
		{
			var submatch = result[i];
			subs[--i] = submatch
				? elm$core$Maybe$Just(submatch)
				: elm$core$Maybe$Nothing;
		}
		out.push(A4(elm$regex$Regex$Match, result[0], result.index, number, _List_fromArray(subs)));
		prevLastIndex = re.lastIndex;
	}
	re.lastIndex = lastIndex;
	return _List_fromArray(out);
});


var _Regex_replaceAtMost = F4(function(n, re, replacer, string)
{
	var count = 0;
	function jsReplacer(match)
	{
		if (count++ >= n)
		{
			return match;
		}
		var i = arguments.length - 3;
		var submatches = new Array(i);
		while (i > 0)
		{
			var submatch = arguments[i];
			submatches[--i] = submatch
				? elm$core$Maybe$Just(submatch)
				: elm$core$Maybe$Nothing;
		}
		return replacer(A4(elm$regex$Regex$Match, match, arguments[arguments.length - 2], count, _List_fromArray(submatches)));
	}
	return string.replace(re, jsReplacer);
});

var _Regex_splitAtMost = F3(function(n, re, str)
{
	var string = str;
	var out = [];
	var start = re.lastIndex;
	var restoreLastIndex = re.lastIndex;
	while (n--)
	{
		var result = re.exec(string);
		if (!result) break;
		out.push(string.slice(start, result.index));
		start = re.lastIndex;
	}
	out.push(string.slice(start));
	re.lastIndex = restoreLastIndex;
	return _List_fromArray(out);
});

var _Regex_infinity = Infinity;




// VIRTUAL-DOM WIDGETS


var _Markdown_toHtml = F3(function(options, factList, rawMarkdown)
{
	return _VirtualDom_custom(
		factList,
		{
			a: options,
			b: rawMarkdown
		},
		_Markdown_render,
		_Markdown_diff
	);
});



// WIDGET IMPLEMENTATION


function _Markdown_render(model)
{
	return A2(_Markdown_replace, model, _VirtualDom_doc.createElement('div'));
}


function _Markdown_diff(x, y)
{
	return x.b === y.b && x.a === y.a
		? false
		: _Markdown_replace(y);
}


var _Markdown_replace = F2(function(model, div)
{
	div.innerHTML = _Markdown_marked(model.b, _Markdown_formatOptions(model.a));
	return div;
});



// ACTUAL MARKDOWN PARSER


var _Markdown_marked = function() {
	// catch the `marked` object regardless of the outer environment.
	// (ex. a CommonJS module compatible environment.)
	// note that this depends on marked's implementation of environment detection.
	var module = {};
	var exports = module.exports = {};

	/**
	 * marked - a markdown parser
	 * Copyright (c) 2011-2014, Christopher Jeffrey. (MIT Licensed)
	 * https://github.com/chjj/marked
	 * commit cd2f6f5b7091154c5526e79b5f3bfb4d15995a51
	 */
	(function(){var block={newline:/^\n+/,code:/^( {4}[^\n]+\n*)+/,fences:noop,hr:/^( *[-*_]){3,} *(?:\n+|$)/,heading:/^ *(#{1,6}) *([^\n]+?) *#* *(?:\n+|$)/,nptable:noop,lheading:/^([^\n]+)\n *(=|-){2,} *(?:\n+|$)/,blockquote:/^( *>[^\n]+(\n(?!def)[^\n]+)*\n*)+/,list:/^( *)(bull) [\s\S]+?(?:hr|def|\n{2,}(?! )(?!\1bull )\n*|\s*$)/,html:/^ *(?:comment *(?:\n|\s*$)|closed *(?:\n{2,}|\s*$)|closing *(?:\n{2,}|\s*$))/,def:/^ *\[([^\]]+)\]: *<?([^\s>]+)>?(?: +["(]([^\n]+)[")])? *(?:\n+|$)/,table:noop,paragraph:/^((?:[^\n]+\n?(?!hr|heading|lheading|blockquote|tag|def))+)\n*/,text:/^[^\n]+/};block.bullet=/(?:[*+-]|\d+\.)/;block.item=/^( *)(bull) [^\n]*(?:\n(?!\1bull )[^\n]*)*/;block.item=replace(block.item,"gm")(/bull/g,block.bullet)();block.list=replace(block.list)(/bull/g,block.bullet)("hr","\\n+(?=\\1?(?:[-*_] *){3,}(?:\\n+|$))")("def","\\n+(?="+block.def.source+")")();block.blockquote=replace(block.blockquote)("def",block.def)();block._tag="(?!(?:"+"a|em|strong|small|s|cite|q|dfn|abbr|data|time|code"+"|var|samp|kbd|sub|sup|i|b|u|mark|ruby|rt|rp|bdi|bdo"+"|span|br|wbr|ins|del|img)\\b)\\w+(?!:/|[^\\w\\s@]*@)\\b";block.html=replace(block.html)("comment",/<!--[\s\S]*?-->/)("closed",/<(tag)[\s\S]+?<\/\1>/)("closing",/<tag(?:"[^"]*"|'[^']*'|[^'">])*?>/)(/tag/g,block._tag)();block.paragraph=replace(block.paragraph)("hr",block.hr)("heading",block.heading)("lheading",block.lheading)("blockquote",block.blockquote)("tag","<"+block._tag)("def",block.def)();block.normal=merge({},block);block.gfm=merge({},block.normal,{fences:/^ *(`{3,}|~{3,})[ \.]*(\S+)? *\n([\s\S]*?)\s*\1 *(?:\n+|$)/,paragraph:/^/,heading:/^ *(#{1,6}) +([^\n]+?) *#* *(?:\n+|$)/});block.gfm.paragraph=replace(block.paragraph)("(?!","(?!"+block.gfm.fences.source.replace("\\1","\\2")+"|"+block.list.source.replace("\\1","\\3")+"|")();block.tables=merge({},block.gfm,{nptable:/^ *(\S.*\|.*)\n *([-:]+ *\|[-| :]*)\n((?:.*\|.*(?:\n|$))*)\n*/,table:/^ *\|(.+)\n *\|( *[-:]+[-| :]*)\n((?: *\|.*(?:\n|$))*)\n*/});function Lexer(options){this.tokens=[];this.tokens.links={};this.options=options||marked.defaults;this.rules=block.normal;if(this.options.gfm){if(this.options.tables){this.rules=block.tables}else{this.rules=block.gfm}}}Lexer.rules=block;Lexer.lex=function(src,options){var lexer=new Lexer(options);return lexer.lex(src)};Lexer.prototype.lex=function(src){src=src.replace(/\r\n|\r/g,"\n").replace(/\t/g,"    ").replace(/\u00a0/g," ").replace(/\u2424/g,"\n");return this.token(src,true)};Lexer.prototype.token=function(src,top,bq){var src=src.replace(/^ +$/gm,""),next,loose,cap,bull,b,item,space,i,l;while(src){if(cap=this.rules.newline.exec(src)){src=src.substring(cap[0].length);if(cap[0].length>1){this.tokens.push({type:"space"})}}if(cap=this.rules.code.exec(src)){src=src.substring(cap[0].length);cap=cap[0].replace(/^ {4}/gm,"");this.tokens.push({type:"code",text:!this.options.pedantic?cap.replace(/\n+$/,""):cap});continue}if(cap=this.rules.fences.exec(src)){src=src.substring(cap[0].length);this.tokens.push({type:"code",lang:cap[2],text:cap[3]||""});continue}if(cap=this.rules.heading.exec(src)){src=src.substring(cap[0].length);this.tokens.push({type:"heading",depth:cap[1].length,text:cap[2]});continue}if(top&&(cap=this.rules.nptable.exec(src))){src=src.substring(cap[0].length);item={type:"table",header:cap[1].replace(/^ *| *\| *$/g,"").split(/ *\| */),align:cap[2].replace(/^ *|\| *$/g,"").split(/ *\| */),cells:cap[3].replace(/\n$/,"").split("\n")};for(i=0;i<item.align.length;i++){if(/^ *-+: *$/.test(item.align[i])){item.align[i]="right"}else if(/^ *:-+: *$/.test(item.align[i])){item.align[i]="center"}else if(/^ *:-+ *$/.test(item.align[i])){item.align[i]="left"}else{item.align[i]=null}}for(i=0;i<item.cells.length;i++){item.cells[i]=item.cells[i].split(/ *\| */)}this.tokens.push(item);continue}if(cap=this.rules.lheading.exec(src)){src=src.substring(cap[0].length);this.tokens.push({type:"heading",depth:cap[2]==="="?1:2,text:cap[1]});continue}if(cap=this.rules.hr.exec(src)){src=src.substring(cap[0].length);this.tokens.push({type:"hr"});continue}if(cap=this.rules.blockquote.exec(src)){src=src.substring(cap[0].length);this.tokens.push({type:"blockquote_start"});cap=cap[0].replace(/^ *> ?/gm,"");this.token(cap,top,true);this.tokens.push({type:"blockquote_end"});continue}if(cap=this.rules.list.exec(src)){src=src.substring(cap[0].length);bull=cap[2];this.tokens.push({type:"list_start",ordered:bull.length>1});cap=cap[0].match(this.rules.item);next=false;l=cap.length;i=0;for(;i<l;i++){item=cap[i];space=item.length;item=item.replace(/^ *([*+-]|\d+\.) +/,"");if(~item.indexOf("\n ")){space-=item.length;item=!this.options.pedantic?item.replace(new RegExp("^ {1,"+space+"}","gm"),""):item.replace(/^ {1,4}/gm,"")}if(this.options.smartLists&&i!==l-1){b=block.bullet.exec(cap[i+1])[0];if(bull!==b&&!(bull.length>1&&b.length>1)){src=cap.slice(i+1).join("\n")+src;i=l-1}}loose=next||/\n\n(?!\s*$)/.test(item);if(i!==l-1){next=item.charAt(item.length-1)==="\n";if(!loose)loose=next}this.tokens.push({type:loose?"loose_item_start":"list_item_start"});this.token(item,false,bq);this.tokens.push({type:"list_item_end"})}this.tokens.push({type:"list_end"});continue}if(cap=this.rules.html.exec(src)){src=src.substring(cap[0].length);this.tokens.push({type:this.options.sanitize?"paragraph":"html",pre:!this.options.sanitizer&&(cap[1]==="pre"||cap[1]==="script"||cap[1]==="style"),text:cap[0]});continue}if(!bq&&top&&(cap=this.rules.def.exec(src))){src=src.substring(cap[0].length);this.tokens.links[cap[1].toLowerCase()]={href:cap[2],title:cap[3]};continue}if(top&&(cap=this.rules.table.exec(src))){src=src.substring(cap[0].length);item={type:"table",header:cap[1].replace(/^ *| *\| *$/g,"").split(/ *\| */),align:cap[2].replace(/^ *|\| *$/g,"").split(/ *\| */),cells:cap[3].replace(/(?: *\| *)?\n$/,"").split("\n")};for(i=0;i<item.align.length;i++){if(/^ *-+: *$/.test(item.align[i])){item.align[i]="right"}else if(/^ *:-+: *$/.test(item.align[i])){item.align[i]="center"}else if(/^ *:-+ *$/.test(item.align[i])){item.align[i]="left"}else{item.align[i]=null}}for(i=0;i<item.cells.length;i++){item.cells[i]=item.cells[i].replace(/^ *\| *| *\| *$/g,"").split(/ *\| */)}this.tokens.push(item);continue}if(top&&(cap=this.rules.paragraph.exec(src))){src=src.substring(cap[0].length);this.tokens.push({type:"paragraph",text:cap[1].charAt(cap[1].length-1)==="\n"?cap[1].slice(0,-1):cap[1]});continue}if(cap=this.rules.text.exec(src)){src=src.substring(cap[0].length);this.tokens.push({type:"text",text:cap[0]});continue}if(src){throw new Error("Infinite loop on byte: "+src.charCodeAt(0))}}return this.tokens};var inline={escape:/^\\([\\`*{}\[\]()#+\-.!_>])/,autolink:/^<([^ >]+(@|:\/)[^ >]+)>/,url:noop,tag:/^<!--[\s\S]*?-->|^<\/?\w+(?:"[^"]*"|'[^']*'|[^'">])*?>/,link:/^!?\[(inside)\]\(href\)/,reflink:/^!?\[(inside)\]\s*\[([^\]]*)\]/,nolink:/^!?\[((?:\[[^\]]*\]|[^\[\]])*)\]/,strong:/^_\_([\s\S]+?)_\_(?!_)|^\*\*([\s\S]+?)\*\*(?!\*)/,em:/^\b_((?:[^_]|_\_)+?)_\b|^\*((?:\*\*|[\s\S])+?)\*(?!\*)/,code:/^(`+)\s*([\s\S]*?[^`])\s*\1(?!`)/,br:/^ {2,}\n(?!\s*$)/,del:noop,text:/^[\s\S]+?(?=[\\<!\[_*`]| {2,}\n|$)/};inline._inside=/(?:\[[^\]]*\]|[^\[\]]|\](?=[^\[]*\]))*/;inline._href=/\s*<?([\s\S]*?)>?(?:\s+['"]([\s\S]*?)['"])?\s*/;inline.link=replace(inline.link)("inside",inline._inside)("href",inline._href)();inline.reflink=replace(inline.reflink)("inside",inline._inside)();inline.normal=merge({},inline);inline.pedantic=merge({},inline.normal,{strong:/^_\_(?=\S)([\s\S]*?\S)_\_(?!_)|^\*\*(?=\S)([\s\S]*?\S)\*\*(?!\*)/,em:/^_(?=\S)([\s\S]*?\S)_(?!_)|^\*(?=\S)([\s\S]*?\S)\*(?!\*)/});inline.gfm=merge({},inline.normal,{escape:replace(inline.escape)("])","~|])")(),url:/^(https?:\/\/[^\s<]+[^<.,:;"')\]\s])/,del:/^~~(?=\S)([\s\S]*?\S)~~/,text:replace(inline.text)("]|","~]|")("|","|https?://|")()});inline.breaks=merge({},inline.gfm,{br:replace(inline.br)("{2,}","*")(),text:replace(inline.gfm.text)("{2,}","*")()});function InlineLexer(links,options){this.options=options||marked.defaults;this.links=links;this.rules=inline.normal;this.renderer=this.options.renderer||new Renderer;this.renderer.options=this.options;if(!this.links){throw new Error("Tokens array requires a `links` property.")}if(this.options.gfm){if(this.options.breaks){this.rules=inline.breaks}else{this.rules=inline.gfm}}else if(this.options.pedantic){this.rules=inline.pedantic}}InlineLexer.rules=inline;InlineLexer.output=function(src,links,options){var inline=new InlineLexer(links,options);return inline.output(src)};InlineLexer.prototype.output=function(src){var out="",link,text,href,cap;while(src){if(cap=this.rules.escape.exec(src)){src=src.substring(cap[0].length);out+=cap[1];continue}if(cap=this.rules.autolink.exec(src)){src=src.substring(cap[0].length);if(cap[2]==="@"){text=cap[1].charAt(6)===":"?this.mangle(cap[1].substring(7)):this.mangle(cap[1]);href=this.mangle("mailto:")+text}else{text=escape(cap[1]);href=text}out+=this.renderer.link(href,null,text);continue}if(!this.inLink&&(cap=this.rules.url.exec(src))){src=src.substring(cap[0].length);text=escape(cap[1]);href=text;out+=this.renderer.link(href,null,text);continue}if(cap=this.rules.tag.exec(src)){if(!this.inLink&&/^<a /i.test(cap[0])){this.inLink=true}else if(this.inLink&&/^<\/a>/i.test(cap[0])){this.inLink=false}src=src.substring(cap[0].length);out+=this.options.sanitize?this.options.sanitizer?this.options.sanitizer(cap[0]):escape(cap[0]):cap[0];continue}if(cap=this.rules.link.exec(src)){src=src.substring(cap[0].length);this.inLink=true;out+=this.outputLink(cap,{href:cap[2],title:cap[3]});this.inLink=false;continue}if((cap=this.rules.reflink.exec(src))||(cap=this.rules.nolink.exec(src))){src=src.substring(cap[0].length);link=(cap[2]||cap[1]).replace(/\s+/g," ");link=this.links[link.toLowerCase()];if(!link||!link.href){out+=cap[0].charAt(0);src=cap[0].substring(1)+src;continue}this.inLink=true;out+=this.outputLink(cap,link);this.inLink=false;continue}if(cap=this.rules.strong.exec(src)){src=src.substring(cap[0].length);out+=this.renderer.strong(this.output(cap[2]||cap[1]));continue}if(cap=this.rules.em.exec(src)){src=src.substring(cap[0].length);out+=this.renderer.em(this.output(cap[2]||cap[1]));continue}if(cap=this.rules.code.exec(src)){src=src.substring(cap[0].length);out+=this.renderer.codespan(escape(cap[2],true));continue}if(cap=this.rules.br.exec(src)){src=src.substring(cap[0].length);out+=this.renderer.br();continue}if(cap=this.rules.del.exec(src)){src=src.substring(cap[0].length);out+=this.renderer.del(this.output(cap[1]));continue}if(cap=this.rules.text.exec(src)){src=src.substring(cap[0].length);out+=this.renderer.text(escape(this.smartypants(cap[0])));continue}if(src){throw new Error("Infinite loop on byte: "+src.charCodeAt(0))}}return out};InlineLexer.prototype.outputLink=function(cap,link){var href=escape(link.href),title=link.title?escape(link.title):null;return cap[0].charAt(0)!=="!"?this.renderer.link(href,title,this.output(cap[1])):this.renderer.image(href,title,escape(cap[1]))};InlineLexer.prototype.smartypants=function(text){if(!this.options.smartypants)return text;return text.replace(/---/g,"—").replace(/--/g,"–").replace(/(^|[-\u2014\/(\[{"\s])'/g,"$1‘").replace(/'/g,"’").replace(/(^|[-\u2014\/(\[{\u2018\s])"/g,"$1“").replace(/"/g,"”").replace(/\.{3}/g,"…")};InlineLexer.prototype.mangle=function(text){if(!this.options.mangle)return text;var out="",l=text.length,i=0,ch;for(;i<l;i++){ch=text.charCodeAt(i);if(Math.random()>.5){ch="x"+ch.toString(16)}out+="&#"+ch+";"}return out};function Renderer(options){this.options=options||{}}Renderer.prototype.code=function(code,lang,escaped){if(this.options.highlight){var out=this.options.highlight(code,lang);if(out!=null&&out!==code){escaped=true;code=out}}if(!lang){return"<pre><code>"+(escaped?code:escape(code,true))+"\n</code></pre>"}return'<pre><code class="'+this.options.langPrefix+escape(lang,true)+'">'+(escaped?code:escape(code,true))+"\n</code></pre>\n"};Renderer.prototype.blockquote=function(quote){return"<blockquote>\n"+quote+"</blockquote>\n"};Renderer.prototype.html=function(html){return html};Renderer.prototype.heading=function(text,level,raw){return"<h"+level+' id="'+this.options.headerPrefix+raw.toLowerCase().replace(/[^\w]+/g,"-")+'">'+text+"</h"+level+">\n"};Renderer.prototype.hr=function(){return this.options.xhtml?"<hr/>\n":"<hr>\n"};Renderer.prototype.list=function(body,ordered){var type=ordered?"ol":"ul";return"<"+type+">\n"+body+"</"+type+">\n"};Renderer.prototype.listitem=function(text){return"<li>"+text+"</li>\n"};Renderer.prototype.paragraph=function(text){return"<p>"+text+"</p>\n"};Renderer.prototype.table=function(header,body){return"<table>\n"+"<thead>\n"+header+"</thead>\n"+"<tbody>\n"+body+"</tbody>\n"+"</table>\n"};Renderer.prototype.tablerow=function(content){return"<tr>\n"+content+"</tr>\n"};Renderer.prototype.tablecell=function(content,flags){var type=flags.header?"th":"td";var tag=flags.align?"<"+type+' style="text-align:'+flags.align+'">':"<"+type+">";return tag+content+"</"+type+">\n"};Renderer.prototype.strong=function(text){return"<strong>"+text+"</strong>"};Renderer.prototype.em=function(text){return"<em>"+text+"</em>"};Renderer.prototype.codespan=function(text){return"<code>"+text+"</code>"};Renderer.prototype.br=function(){return this.options.xhtml?"<br/>":"<br>"};Renderer.prototype.del=function(text){return"<del>"+text+"</del>"};Renderer.prototype.link=function(href,title,text){if(this.options.sanitize){try{var prot=decodeURIComponent(unescape(href)).replace(/[^\w:]/g,"").toLowerCase()}catch(e){return""}if(prot.indexOf("javascript:")===0||prot.indexOf("vbscript:")===0||prot.indexOf("data:")===0){return""}}var out='<a href="'+href+'"';if(title){out+=' title="'+title+'"'}out+=">"+text+"</a>";return out};Renderer.prototype.image=function(href,title,text){var out='<img src="'+href+'" alt="'+text+'"';if(title){out+=' title="'+title+'"'}out+=this.options.xhtml?"/>":">";return out};Renderer.prototype.text=function(text){return text};function Parser(options){this.tokens=[];this.token=null;this.options=options||marked.defaults;this.options.renderer=this.options.renderer||new Renderer;this.renderer=this.options.renderer;this.renderer.options=this.options}Parser.parse=function(src,options,renderer){var parser=new Parser(options,renderer);return parser.parse(src)};Parser.prototype.parse=function(src){this.inline=new InlineLexer(src.links,this.options,this.renderer);this.tokens=src.reverse();var out="";while(this.next()){out+=this.tok()}return out};Parser.prototype.next=function(){return this.token=this.tokens.pop()};Parser.prototype.peek=function(){return this.tokens[this.tokens.length-1]||0};Parser.prototype.parseText=function(){var body=this.token.text;while(this.peek().type==="text"){body+="\n"+this.next().text}return this.inline.output(body)};Parser.prototype.tok=function(){switch(this.token.type){case"space":{return""}case"hr":{return this.renderer.hr()}case"heading":{return this.renderer.heading(this.inline.output(this.token.text),this.token.depth,this.token.text)}case"code":{return this.renderer.code(this.token.text,this.token.lang,this.token.escaped)}case"table":{var header="",body="",i,row,cell,flags,j;cell="";for(i=0;i<this.token.header.length;i++){flags={header:true,align:this.token.align[i]};cell+=this.renderer.tablecell(this.inline.output(this.token.header[i]),{header:true,align:this.token.align[i]})}header+=this.renderer.tablerow(cell);for(i=0;i<this.token.cells.length;i++){row=this.token.cells[i];cell="";for(j=0;j<row.length;j++){cell+=this.renderer.tablecell(this.inline.output(row[j]),{header:false,align:this.token.align[j]})}body+=this.renderer.tablerow(cell)}return this.renderer.table(header,body)}case"blockquote_start":{var body="";while(this.next().type!=="blockquote_end"){body+=this.tok()}return this.renderer.blockquote(body)}case"list_start":{var body="",ordered=this.token.ordered;while(this.next().type!=="list_end"){body+=this.tok()}return this.renderer.list(body,ordered)}case"list_item_start":{var body="";while(this.next().type!=="list_item_end"){body+=this.token.type==="text"?this.parseText():this.tok()}return this.renderer.listitem(body)}case"loose_item_start":{var body="";while(this.next().type!=="list_item_end"){body+=this.tok()}return this.renderer.listitem(body)}case"html":{var html=!this.token.pre&&!this.options.pedantic?this.inline.output(this.token.text):this.token.text;return this.renderer.html(html)}case"paragraph":{return this.renderer.paragraph(this.inline.output(this.token.text))}case"text":{return this.renderer.paragraph(this.parseText())}}};function escape(html,encode){return html.replace(!encode?/&(?!#?\w+;)/g:/&/g,"&amp;").replace(/</g,"&lt;").replace(/>/g,"&gt;").replace(/"/g,"&quot;").replace(/'/g,"&#39;")}function unescape(html){return html.replace(/&(#(?:\d+)|(?:#x[0-9A-Fa-f]+)|(?:\w+));?/g,function(_,n){n=n.toLowerCase();if(n==="colon")return":";if(n.charAt(0)==="#"){return n.charAt(1)==="x"?String.fromCharCode(parseInt(n.substring(2),16)):String.fromCharCode(+n.substring(1))}return""})}function replace(regex,opt){regex=regex.source;opt=opt||"";return function self(name,val){if(!name)return new RegExp(regex,opt);val=val.source||val;val=val.replace(/(^|[^\[])\^/g,"$1");regex=regex.replace(name,val);return self}}function noop(){}noop.exec=noop;function merge(obj){var i=1,target,key;for(;i<arguments.length;i++){target=arguments[i];for(key in target){if(Object.prototype.hasOwnProperty.call(target,key)){obj[key]=target[key]}}}return obj}function marked(src,opt,callback){if(callback||typeof opt==="function"){if(!callback){callback=opt;opt=null}opt=merge({},marked.defaults,opt||{});var highlight=opt.highlight,tokens,pending,i=0;try{tokens=Lexer.lex(src,opt)}catch(e){return callback(e)}pending=tokens.length;var done=function(err){if(err){opt.highlight=highlight;return callback(err)}var out;try{out=Parser.parse(tokens,opt)}catch(e){err=e}opt.highlight=highlight;return err?callback(err):callback(null,out)};if(!highlight||highlight.length<3){return done()}delete opt.highlight;if(!pending)return done();for(;i<tokens.length;i++){(function(token){if(token.type!=="code"){return--pending||done()}return highlight(token.text,token.lang,function(err,code){if(err)return done(err);if(code==null||code===token.text){return--pending||done()}token.text=code;token.escaped=true;--pending||done()})})(tokens[i])}return}try{if(opt)opt=merge({},marked.defaults,opt);return Parser.parse(Lexer.lex(src,opt),opt)}catch(e){e.message+="\nPlease report this to https://github.com/chjj/marked.";if((opt||marked.defaults).silent){return"<p>An error occured:</p><pre>"+escape(e.message+"",true)+"</pre>"}throw e}}marked.options=marked.setOptions=function(opt){merge(marked.defaults,opt);return marked};marked.defaults={gfm:true,tables:true,breaks:false,pedantic:false,sanitize:false,sanitizer:null,mangle:true,smartLists:false,silent:false,highlight:null,langPrefix:"lang-",smartypants:false,headerPrefix:"",renderer:new Renderer,xhtml:false};marked.Parser=Parser;marked.parser=Parser.parse;marked.Renderer=Renderer;marked.Lexer=Lexer;marked.lexer=Lexer.lex;marked.InlineLexer=InlineLexer;marked.inlineLexer=InlineLexer.output;marked.parse=marked;if(typeof module!=="undefined"&&typeof exports==="object"){module.exports=marked}else if(typeof define==="function"&&define.amd){define(function(){return marked})}else{this.marked=marked}}).call(function(){return this||(typeof window!=="undefined"?window:global)}());

	return module.exports;
}();


// FORMAT OPTIONS FOR MARKED IMPLEMENTATION

function _Markdown_formatOptions(options)
{
	function toHighlight(code, lang)
	{
		if (!lang && elm$core$Maybe$isJust(options.a$))
		{
			lang = options.a$.a;
		}

		if (typeof hljs !== 'undefined' && lang && hljs.listLanguages().indexOf(lang) >= 0)
		{
			return hljs.highlight(lang, code, true).value;
		}

		return code;
	}

	var gfm = options.a9.a;

	return {
		highlight: toHighlight,
		gfm: gfm,
		tables: gfm && gfm.cq,
		breaks: gfm && gfm.bW,
		sanitize: options.bx,
		smartypants: options.bA
	};
}
var elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var elm$core$Array$branchFactor = 32;
var elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
	});
var elm$core$Basics$EQ = 1;
var elm$core$Basics$GT = 2;
var elm$core$Basics$LT = 0;
var elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === -2) {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3(elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var elm$core$List$cons = _List_cons;
var elm$core$Dict$toList = function (dict) {
	return A3(
		elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var elm$core$Dict$keys = function (dict) {
	return A3(
		elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2(elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var elm$core$Set$toList = function (_n0) {
	var dict = _n0;
	return elm$core$Dict$keys(dict);
};
var elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var elm$core$Array$foldr = F3(
	function (func, baseCase, _n0) {
		var tree = _n0.c;
		var tail = _n0.d;
		var helper = F2(
			function (node, acc) {
				if (!node.$) {
					var subTree = node.a;
					return A3(elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3(elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			elm$core$Elm$JsArray$foldr,
			helper,
			A3(elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var elm$core$Array$toList = function (array) {
	return A3(elm$core$Array$foldr, elm$core$List$cons, _List_Nil, array);
};
var elm$core$Basics$ceiling = _Basics_ceiling;
var elm$core$Basics$fdiv = _Basics_fdiv;
var elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var elm$core$Basics$toFloat = _Basics_toFloat;
var elm$core$Array$shiftStep = elm$core$Basics$ceiling(
	A2(elm$core$Basics$logBase, 2, elm$core$Array$branchFactor));
var elm$core$Elm$JsArray$empty = _JsArray_empty;
var elm$core$Array$empty = A4(elm$core$Array$Array_elm_builtin, 0, elm$core$Array$shiftStep, elm$core$Elm$JsArray$empty, elm$core$Elm$JsArray$empty);
var elm$core$Array$Leaf = function (a) {
	return {$: 1, a: a};
};
var elm$core$Array$SubTree = function (a) {
	return {$: 0, a: a};
};
var elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var elm$core$List$reverse = function (list) {
	return A3(elm$core$List$foldl, elm$core$List$cons, _List_Nil, list);
};
var elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _n0 = A2(elm$core$Elm$JsArray$initializeFromList, elm$core$Array$branchFactor, nodes);
			var node = _n0.a;
			var remainingNodes = _n0.b;
			var newAcc = A2(
				elm$core$List$cons,
				elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var elm$core$Basics$eq = _Utils_equal;
var elm$core$Tuple$first = function (_n0) {
	var x = _n0.a;
	return x;
};
var elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = elm$core$Basics$ceiling(nodeListSize / elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2(elm$core$Elm$JsArray$initializeFromList, elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2(elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var elm$core$Basics$add = _Basics_add;
var elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var elm$core$Basics$floor = _Basics_floor;
var elm$core$Basics$gt = _Utils_gt;
var elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var elm$core$Basics$mul = _Basics_mul;
var elm$core$Basics$sub = _Basics_sub;
var elm$core$Elm$JsArray$length = _JsArray_length;
var elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.a) {
			return A4(
				elm$core$Array$Array_elm_builtin,
				elm$core$Elm$JsArray$length(builder.b),
				elm$core$Array$shiftStep,
				elm$core$Elm$JsArray$empty,
				builder.b);
		} else {
			var treeLen = builder.a * elm$core$Array$branchFactor;
			var depth = elm$core$Basics$floor(
				A2(elm$core$Basics$logBase, elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? elm$core$List$reverse(builder.c) : builder.c;
			var tree = A2(elm$core$Array$treeFromBuilder, correctNodeList, builder.a);
			return A4(
				elm$core$Array$Array_elm_builtin,
				elm$core$Elm$JsArray$length(builder.b) + treeLen,
				A2(elm$core$Basics$max, 5, depth * elm$core$Array$shiftStep),
				tree,
				builder.b);
		}
	});
var elm$core$Basics$False = 1;
var elm$core$Basics$idiv = _Basics_idiv;
var elm$core$Basics$lt = _Utils_lt;
var elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					elm$core$Array$builderToArray,
					false,
					{c: nodeList, a: (len / elm$core$Array$branchFactor) | 0, b: tail});
			} else {
				var leaf = elm$core$Array$Leaf(
					A3(elm$core$Elm$JsArray$initialize, elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2(elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var elm$core$Basics$le = _Utils_le;
var elm$core$Basics$remainderBy = _Basics_remainderBy;
var elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return elm$core$Array$empty;
		} else {
			var tailLen = len % elm$core$Array$branchFactor;
			var tail = A3(elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - elm$core$Array$branchFactor;
			return A5(elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var elm$core$Maybe$Just = function (a) {
	return {$: 0, a: a};
};
var elm$core$Maybe$Nothing = {$: 1};
var elm$core$Result$Err = function (a) {
	return {$: 1, a: a};
};
var elm$core$Result$Ok = function (a) {
	return {$: 0, a: a};
};
var elm$core$Basics$True = 0;
var elm$core$Result$isOk = function (result) {
	if (!result.$) {
		return true;
	} else {
		return false;
	}
};
var elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var elm$json$Json$Decode$OneOf = function (a) {
	return {$: 2, a: a};
};
var elm$core$Basics$and = _Basics_and;
var elm$core$Basics$append = _Utils_append;
var elm$core$Basics$or = _Basics_or;
var elm$core$Char$toCode = _Char_toCode;
var elm$core$Char$isLower = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var elm$core$Char$isUpper = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var elm$core$Char$isAlpha = function (_char) {
	return elm$core$Char$isLower(_char) || elm$core$Char$isUpper(_char);
};
var elm$core$Char$isDigit = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var elm$core$Char$isAlphaNum = function (_char) {
	return elm$core$Char$isLower(_char) || (elm$core$Char$isUpper(_char) || elm$core$Char$isDigit(_char));
};
var elm$core$List$length = function (xs) {
	return A3(
		elm$core$List$foldl,
		F2(
			function (_n0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var elm$core$List$map2 = _List_map2;
var elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2(elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var elm$core$List$range = F2(
	function (lo, hi) {
		return A3(elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			elm$core$List$map2,
			f,
			A2(
				elm$core$List$range,
				0,
				elm$core$List$length(xs) - 1),
			xs);
	});
var elm$core$String$all = _String_all;
var elm$core$String$fromInt = _String_fromNumber;
var elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var elm$core$String$uncons = _String_uncons;
var elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var elm$json$Json$Decode$indent = function (str) {
	return A2(
		elm$core$String$join,
		'\n    ',
		A2(elm$core$String$split, '\n', str));
};
var elm$json$Json$Encode$encode = _Json_encode;
var elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + (elm$core$String$fromInt(i + 1) + (') ' + elm$json$Json$Decode$indent(
			elm$json$Json$Decode$errorToString(error))));
	});
var elm$json$Json$Decode$errorToString = function (error) {
	return A2(elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 0:
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _n1 = elm$core$String$uncons(f);
						if (_n1.$ === 1) {
							return false;
						} else {
							var _n2 = _n1.a;
							var _char = _n2.a;
							var rest = _n2.b;
							return elm$core$Char$isAlpha(_char) && A2(elm$core$String$all, elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2(elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 1:
					var i = error.a;
					var err = error.b;
					var indexName = '[' + (elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2(elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 2:
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									elm$core$String$join,
									'',
									elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										elm$core$String$join,
										'',
										elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + (elm$core$String$fromInt(
								elm$core$List$length(errors)) + ' ways:'));
							return A2(
								elm$core$String$join,
								'\n\n',
								A2(
									elm$core$List$cons,
									introduction,
									A2(elm$core$List$indexedMap, elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								elm$core$String$join,
								'',
								elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + (elm$json$Json$Decode$indent(
						A2(elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var elm$json$Json$Decode$map2 = _Json_map2;
var NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$custom = elm$json$Json$Decode$map2(elm$core$Basics$apR);
var elm$json$Json$Decode$field = _Json_decodeField;
var NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required = F3(
	function (key, valDecoder, decoder) {
		return A2(
			NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$custom,
			A2(elm$json$Json$Decode$field, key, valDecoder),
			decoder);
	});
var author$project$Api$Cred = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var author$project$Username$Username = elm$core$Basics$identity;
var elm$core$Basics$identity = function (x) {
	return x;
};
var elm$json$Json$Decode$map = _Json_map1;
var elm$json$Json$Decode$string = _Json_decodeString;
var author$project$Username$decoder = A2(elm$json$Json$Decode$map, elm$core$Basics$identity, elm$json$Json$Decode$string);
var elm$json$Json$Decode$succeed = _Json_succeed;
var author$project$Api$credDecoder = A3(
	NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
	'token',
	elm$json$Json$Decode$string,
	A3(
		NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
		'username',
		author$project$Username$decoder,
		elm$json$Json$Decode$succeed(author$project$Api$Cred)));
var author$project$Api$decoderFromCred = function (decoder) {
	return A3(elm$json$Json$Decode$map2, elm$core$Basics$identity, decoder, author$project$Api$credDecoder);
};
var author$project$Api$storageDecoder = function (viewerDecoder) {
	return A2(
		elm$json$Json$Decode$field,
		'user',
		author$project$Api$decoderFromCred(viewerDecoder));
};
var elm$browser$Browser$External = function (a) {
	return {$: 1, a: a};
};
var elm$browser$Browser$Internal = function (a) {
	return {$: 0, a: a};
};
var elm$browser$Browser$Dom$NotFound = elm$core$Basics$identity;
var elm$core$Basics$never = function (_n0) {
	never:
	while (true) {
		var nvr = _n0;
		var $temp$_n0 = nvr;
		_n0 = $temp$_n0;
		continue never;
	}
};
var elm$core$Task$Perform = elm$core$Basics$identity;
var elm$core$Task$succeed = _Scheduler_succeed;
var elm$core$Task$init = elm$core$Task$succeed(0);
var elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							elm$core$List$foldl,
							fn,
							acc,
							elm$core$List$reverse(r4)) : A4(elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4(elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var elm$core$Task$andThen = _Scheduler_andThen;
var elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			elm$core$Task$andThen,
			function (a) {
				return elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			elm$core$Task$andThen,
			function (a) {
				return A2(
					elm$core$Task$andThen,
					function (b) {
						return elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var elm$core$Task$sequence = function (tasks) {
	return A3(
		elm$core$List$foldr,
		elm$core$Task$map2(elm$core$List$cons),
		elm$core$Task$succeed(_List_Nil),
		tasks);
};
var elm$core$Platform$sendToApp = _Platform_sendToApp;
var elm$core$Task$spawnCmd = F2(
	function (router, _n0) {
		var task = _n0;
		return _Scheduler_spawn(
			A2(
				elm$core$Task$andThen,
				elm$core$Platform$sendToApp(router),
				task));
	});
var elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			elm$core$Task$map,
			function (_n0) {
				return 0;
			},
			elm$core$Task$sequence(
				A2(
					elm$core$List$map,
					elm$core$Task$spawnCmd(router),
					commands)));
	});
var elm$core$Task$onSelfMsg = F3(
	function (_n0, _n1, _n2) {
		return elm$core$Task$succeed(0);
	});
var elm$core$Task$cmdMap = F2(
	function (tagger, _n0) {
		var task = _n0;
		return A2(elm$core$Task$map, tagger, task);
	});
_Platform_effectManagers['Task'] = _Platform_createManager(elm$core$Task$init, elm$core$Task$onEffects, elm$core$Task$onSelfMsg, elm$core$Task$cmdMap);
var elm$core$Task$command = _Platform_leaf('Task');
var elm$core$Task$perform = F2(
	function (toMessage, task) {
		return elm$core$Task$command(
			A2(elm$core$Task$map, toMessage, task));
	});
var elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 0:
			return 0;
		case 1:
			return 1;
		case 2:
			return 2;
		default:
			return 3;
	}
};
var elm$core$String$length = _String_length;
var elm$core$String$slice = _String_slice;
var elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			elm$core$String$slice,
			n,
			elm$core$String$length(string),
			string);
	});
var elm$core$String$startsWith = _String_startsWith;
var elm$url$Url$Http = 0;
var elm$url$Url$Https = 1;
var elm$core$String$indexes = _String_indexes;
var elm$core$String$isEmpty = function (string) {
	return string === '';
};
var elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3(elm$core$String$slice, 0, n, string);
	});
var elm$core$String$contains = _String_contains;
var elm$core$String$toInt = _String_toInt;
var elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {b8: fragment, bc: host, ck: path, bp: port_, bt: protocol, bu: query};
	});
var elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if (elm$core$String$isEmpty(str) || A2(elm$core$String$contains, '@', str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, ':', str);
			if (!_n0.b) {
				return elm$core$Maybe$Just(
					A6(elm$url$Url$Url, protocol, str, elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_n0.b.b) {
					var i = _n0.a;
					var _n1 = elm$core$String$toInt(
						A2(elm$core$String$dropLeft, i + 1, str));
					if (_n1.$ === 1) {
						return elm$core$Maybe$Nothing;
					} else {
						var port_ = _n1;
						return elm$core$Maybe$Just(
							A6(
								elm$url$Url$Url,
								protocol,
								A2(elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return elm$core$Maybe$Nothing;
				}
			}
		}
	});
var elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if (elm$core$String$isEmpty(str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, '/', str);
			if (!_n0.b) {
				return A5(elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _n0.a;
				return A5(
					elm$url$Url$chompBeforePath,
					protocol,
					A2(elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2(elm$core$String$left, i, str));
			}
		}
	});
var elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if (elm$core$String$isEmpty(str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, '?', str);
			if (!_n0.b) {
				return A4(elm$url$Url$chompBeforeQuery, protocol, elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _n0.a;
				return A4(
					elm$url$Url$chompBeforeQuery,
					protocol,
					elm$core$Maybe$Just(
						A2(elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2(elm$core$String$left, i, str));
			}
		}
	});
var elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if (elm$core$String$isEmpty(str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, '#', str);
			if (!_n0.b) {
				return A3(elm$url$Url$chompBeforeFragment, protocol, elm$core$Maybe$Nothing, str);
			} else {
				var i = _n0.a;
				return A3(
					elm$url$Url$chompBeforeFragment,
					protocol,
					elm$core$Maybe$Just(
						A2(elm$core$String$dropLeft, i + 1, str)),
					A2(elm$core$String$left, i, str));
			}
		}
	});
var elm$url$Url$fromString = function (str) {
	return A2(elm$core$String$startsWith, 'http://', str) ? A2(
		elm$url$Url$chompAfterProtocol,
		0,
		A2(elm$core$String$dropLeft, 7, str)) : (A2(elm$core$String$startsWith, 'https://', str) ? A2(
		elm$url$Url$chompAfterProtocol,
		1,
		A2(elm$core$String$dropLeft, 8, str)) : elm$core$Maybe$Nothing);
};
var elm$browser$Browser$application = _Browser_application;
var elm$core$Result$andThen = F2(
	function (callback, result) {
		if (!result.$) {
			var value = result.a;
			return callback(value);
		} else {
			var msg = result.a;
			return elm$core$Result$Err(msg);
		}
	});
var elm$core$Result$toMaybe = function (result) {
	if (!result.$) {
		var v = result.a;
		return elm$core$Maybe$Just(v);
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var elm$json$Json$Decode$decodeString = _Json_runOnString;
var elm$json$Json$Decode$decodeValue = _Json_run;
var author$project$Api$application = F2(
	function (viewerDecoder, config) {
		var init = F3(
			function (flags, url, navKey) {
				var maybeViewer = elm$core$Result$toMaybe(
					A2(
						elm$core$Result$andThen,
						elm$json$Json$Decode$decodeString(
							author$project$Api$storageDecoder(viewerDecoder)),
						A2(elm$json$Json$Decode$decodeValue, elm$json$Json$Decode$string, flags)));
				return A3(config.be, maybeViewer, url, navKey);
			});
		return elm$browser$Browser$application(
			{be: init, bl: config.bl, bm: config.bm, bE: config.bE, bI: config.bI, bP: config.bP});
	});
var author$project$Main$ChangedUrl = function (a) {
	return {$: 2, a: a};
};
var author$project$Main$ClickedLink = function (a) {
	return {$: 3, a: a};
};
var author$project$Main$Redirect = function (a) {
	return {$: 0, a: a};
};
var elm$core$Maybe$destruct = F3(
	function (_default, func, maybe) {
		if (!maybe.$) {
			var a = maybe.a;
			return func(a);
		} else {
			return _default;
		}
	});
var elm$json$Json$Encode$null = _Json_encodeNull;
var author$project$Api$storeCache = _Platform_outgoingPort(
	'storeCache',
	function ($) {
		return A3(elm$core$Maybe$destruct, elm$json$Json$Encode$null, elm$core$Basics$identity, $);
	});
var author$project$Api$logout = author$project$Api$storeCache(elm$core$Maybe$Nothing);
var author$project$Main$AccountBalance = function (a) {
	return {$: 6, a: a};
};
var author$project$Main$ApiDocument = function (a) {
	return {$: 8, a: a};
};
var author$project$Main$Charts = function (a) {
	return {$: 5, a: a};
};
var author$project$Main$Dashboard = function (a) {
	return {$: 2, a: a};
};
var author$project$Main$GotAccountBalanceMsg = function (a) {
	return {$: 8, a: a};
};
var author$project$Main$GotApiDocumentMsg = function (a) {
	return {$: 10, a: a};
};
var author$project$Main$GotChartsMsg = function (a) {
	return {$: 7, a: a};
};
var author$project$Main$GotDashboardMsg = function (a) {
	return {$: 4, a: a};
};
var author$project$Main$GotLoginMsg = function (a) {
	return {$: 9, a: a};
};
var author$project$Main$GotPortfolioMsg = function (a) {
	return {$: 6, a: a};
};
var author$project$Main$GotUploadMsg = function (a) {
	return {$: 5, a: a};
};
var author$project$Main$Login = function (a) {
	return {$: 7, a: a};
};
var author$project$Main$NotFound = function (a) {
	return {$: 1, a: a};
};
var author$project$Main$Portfolio = function (a) {
	return {$: 4, a: a};
};
var author$project$Main$Upload = function (a) {
	return {$: 3, a: a};
};
var author$project$Page$AccountBalance$toSession = function (model) {
	return model.ac;
};
var author$project$Page$ApiDocument$toSession = function (model) {
	return model.ac;
};
var author$project$Page$Charts$toSession = function (model) {
	return model.ac;
};
var author$project$Page$Dashboard$toSession = function (model) {
	return model.ac;
};
var author$project$Page$Login$toSession = function (model) {
	return model.ac;
};
var author$project$Page$Portfolio$toSession = function (model) {
	return model.ac;
};
var author$project$Page$Upload$toSession = function (model) {
	return model.ac;
};
var author$project$Main$toSession = function (page) {
	switch (page.$) {
		case 0:
			var session = page.a;
			return session;
		case 1:
			var session = page.a;
			return session;
		case 2:
			var dashboard = page.a;
			return author$project$Page$Dashboard$toSession(dashboard);
		case 3:
			var upload = page.a;
			return author$project$Page$Upload$toSession(upload);
		case 4:
			var portfolio = page.a;
			return author$project$Page$Portfolio$toSession(portfolio);
		case 5:
			var charts = page.a;
			return author$project$Page$Charts$toSession(charts);
		case 6:
			var accbalance = page.a;
			return author$project$Page$AccountBalance$toSession(accbalance);
		case 7:
			var login = page.a;
			return author$project$Page$Login$toSession(login);
		default:
			var login = page.a;
			return author$project$Page$ApiDocument$toSession(login);
	}
};
var elm$core$Platform$Cmd$map = _Platform_map;
var author$project$Main$updateWith = F4(
	function (toModel, toMsg, model, _n0) {
		var subModel = _n0.a;
		var subCmd = _n0.b;
		return _Utils_Tuple2(
			toModel(subModel),
			A2(elm$core$Platform$Cmd$map, toMsg, subCmd));
	});
var elm$core$Platform$Cmd$batch = _Platform_batch;
var elm$core$Platform$Cmd$none = elm$core$Platform$Cmd$batch(_List_Nil);
var author$project$Page$AccountBalance$init = function (session) {
	var model = {v: false, ac: session};
	return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
};
var author$project$Page$ApiDocument$UpdateWebApiDocument = function (a) {
	return {$: 3, a: a};
};
var elm$http$Http$Internal$EmptyBody = {$: 0};
var elm$http$Http$emptyBody = elm$http$Http$Internal$EmptyBody;
var elm$core$Dict$RBEmpty_elm_builtin = {$: -2};
var elm$core$Dict$empty = elm$core$Dict$RBEmpty_elm_builtin;
var elm$core$Basics$compare = _Utils_compare;
var elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === -2) {
				return elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _n1 = A2(elm$core$Basics$compare, targetKey, key);
				switch (_n1) {
					case 0:
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 1:
						return elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var elm$core$Dict$Black = 1;
var elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: -1, a: a, b: b, c: c, d: d, e: e};
	});
var elm$core$Dict$Red = 0;
var elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === -1) && (!right.a)) {
			var _n1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === -1) && (!left.a)) {
				var _n3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					0,
					key,
					value,
					A5(elm$core$Dict$RBNode_elm_builtin, 1, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rLeft, rRight));
			} else {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5(elm$core$Dict$RBNode_elm_builtin, 0, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === -1) && (!left.a)) && (left.d.$ === -1)) && (!left.d.a)) {
				var _n5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _n6 = left.d;
				var _n7 = _n6.a;
				var llK = _n6.b;
				var llV = _n6.c;
				var llLeft = _n6.d;
				var llRight = _n6.e;
				var lRight = left.e;
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					0,
					lK,
					lV,
					A5(elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
					A5(elm$core$Dict$RBNode_elm_builtin, 1, key, value, lRight, right));
			} else {
				return A5(elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === -2) {
			return A5(elm$core$Dict$RBNode_elm_builtin, 0, key, value, elm$core$Dict$RBEmpty_elm_builtin, elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _n1 = A2(elm$core$Basics$compare, key, nKey);
			switch (_n1) {
				case 0:
					return A5(
						elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3(elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 1:
					return A5(elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3(elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _n0 = A3(elm$core$Dict$insertHelp, key, value, dict);
		if ((_n0.$ === -1) && (!_n0.a)) {
			var _n1 = _n0.a;
			var k = _n0.b;
			var v = _n0.c;
			var l = _n0.d;
			var r = _n0.e;
			return A5(elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _n0;
			return x;
		}
	});
var elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === -1) && (dict.d.$ === -1)) {
			var left = dict.d;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var elm$core$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === -1) && (dict.d.$ === -1)) && (dict.e.$ === -1)) {
		if ((dict.e.d.$ === -1) && (!dict.e.d.a)) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _n1 = dict.d;
			var lClr = _n1.a;
			var lK = _n1.b;
			var lV = _n1.c;
			var lLeft = _n1.d;
			var lRight = _n1.e;
			var _n2 = dict.e;
			var rClr = _n2.a;
			var rK = _n2.b;
			var rV = _n2.c;
			var rLeft = _n2.d;
			var _n3 = rLeft.a;
			var rlK = rLeft.b;
			var rlV = rLeft.c;
			var rlL = rLeft.d;
			var rlR = rLeft.e;
			var rRight = _n2.e;
			return A5(
				elm$core$Dict$RBNode_elm_builtin,
				0,
				rlK,
				rlV,
				A5(
					elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					rlL),
				A5(elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rlR, rRight));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _n4 = dict.d;
			var lClr = _n4.a;
			var lK = _n4.b;
			var lV = _n4.c;
			var lLeft = _n4.d;
			var lRight = _n4.e;
			var _n5 = dict.e;
			var rClr = _n5.a;
			var rK = _n5.b;
			var rV = _n5.c;
			var rLeft = _n5.d;
			var rRight = _n5.e;
			if (clr === 1) {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			} else {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === -1) && (dict.d.$ === -1)) && (dict.e.$ === -1)) {
		if ((dict.d.d.$ === -1) && (!dict.d.d.a)) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _n1 = dict.d;
			var lClr = _n1.a;
			var lK = _n1.b;
			var lV = _n1.c;
			var _n2 = _n1.d;
			var _n3 = _n2.a;
			var llK = _n2.b;
			var llV = _n2.c;
			var llLeft = _n2.d;
			var llRight = _n2.e;
			var lRight = _n1.e;
			var _n4 = dict.e;
			var rClr = _n4.a;
			var rK = _n4.b;
			var rV = _n4.c;
			var rLeft = _n4.d;
			var rRight = _n4.e;
			return A5(
				elm$core$Dict$RBNode_elm_builtin,
				0,
				lK,
				lV,
				A5(elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
				A5(
					elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					lRight,
					A5(elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight)));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _n5 = dict.d;
			var lClr = _n5.a;
			var lK = _n5.b;
			var lV = _n5.c;
			var lLeft = _n5.d;
			var lRight = _n5.e;
			var _n6 = dict.e;
			var rClr = _n6.a;
			var rK = _n6.b;
			var rV = _n6.c;
			var rLeft = _n6.d;
			var rRight = _n6.e;
			if (clr === 1) {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			} else {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === -1) && (!left.a)) {
			var _n1 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
				elm$core$Dict$RBNode_elm_builtin,
				color,
				lK,
				lV,
				lLeft,
				A5(elm$core$Dict$RBNode_elm_builtin, 0, key, value, lRight, right));
		} else {
			_n2$2:
			while (true) {
				if ((right.$ === -1) && (right.a === 1)) {
					if (right.d.$ === -1) {
						if (right.d.a === 1) {
							var _n3 = right.a;
							var _n4 = right.d;
							var _n5 = _n4.a;
							return elm$core$Dict$moveRedRight(dict);
						} else {
							break _n2$2;
						}
					} else {
						var _n6 = right.a;
						var _n7 = right.d;
						return elm$core$Dict$moveRedRight(dict);
					}
				} else {
					break _n2$2;
				}
			}
			return dict;
		}
	});
var elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === -1) && (dict.d.$ === -1)) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor === 1) {
			if ((lLeft.$ === -1) && (!lLeft.a)) {
				var _n3 = lLeft.a;
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					color,
					key,
					value,
					elm$core$Dict$removeMin(left),
					right);
			} else {
				var _n4 = elm$core$Dict$moveRedLeft(dict);
				if (_n4.$ === -1) {
					var nColor = _n4.a;
					var nKey = _n4.b;
					var nValue = _n4.c;
					var nLeft = _n4.d;
					var nRight = _n4.e;
					return A5(
						elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						elm$core$Dict$removeMin(nLeft),
						nRight);
				} else {
					return elm$core$Dict$RBEmpty_elm_builtin;
				}
			}
		} else {
			return A5(
				elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				value,
				elm$core$Dict$removeMin(left),
				right);
		}
	} else {
		return elm$core$Dict$RBEmpty_elm_builtin;
	}
};
var elm$core$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === -2) {
			return elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === -1) && (left.a === 1)) {
					var _n4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === -1) && (!lLeft.a)) {
						var _n6 = lLeft.a;
						return A5(
							elm$core$Dict$RBNode_elm_builtin,
							color,
							key,
							value,
							A2(elm$core$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _n7 = elm$core$Dict$moveRedLeft(dict);
						if (_n7.$ === -1) {
							var nColor = _n7.a;
							var nKey = _n7.b;
							var nValue = _n7.c;
							var nLeft = _n7.d;
							var nRight = _n7.e;
							return A5(
								elm$core$Dict$balance,
								nColor,
								nKey,
								nValue,
								A2(elm$core$Dict$removeHelp, targetKey, nLeft),
								nRight);
						} else {
							return elm$core$Dict$RBEmpty_elm_builtin;
						}
					}
				} else {
					return A5(
						elm$core$Dict$RBNode_elm_builtin,
						color,
						key,
						value,
						A2(elm$core$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					elm$core$Dict$removeHelpEQGT,
					targetKey,
					A7(elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var elm$core$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === -1) {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _n1 = elm$core$Dict$getMin(right);
				if (_n1.$ === -1) {
					var minKey = _n1.b;
					var minValue = _n1.c;
					return A5(
						elm$core$Dict$balance,
						color,
						minKey,
						minValue,
						left,
						elm$core$Dict$removeMin(right));
				} else {
					return elm$core$Dict$RBEmpty_elm_builtin;
				}
			} else {
				return A5(
					elm$core$Dict$balance,
					color,
					key,
					value,
					left,
					A2(elm$core$Dict$removeHelp, targetKey, right));
			}
		} else {
			return elm$core$Dict$RBEmpty_elm_builtin;
		}
	});
var elm$core$Dict$remove = F2(
	function (key, dict) {
		var _n0 = A2(elm$core$Dict$removeHelp, key, dict);
		if ((_n0.$ === -1) && (!_n0.a)) {
			var _n1 = _n0.a;
			var k = _n0.b;
			var v = _n0.c;
			var l = _n0.d;
			var r = _n0.e;
			return A5(elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _n0;
			return x;
		}
	});
var elm$core$Dict$update = F3(
	function (targetKey, alter, dictionary) {
		var _n0 = alter(
			A2(elm$core$Dict$get, targetKey, dictionary));
		if (!_n0.$) {
			var value = _n0.a;
			return A3(elm$core$Dict$insert, targetKey, value, dictionary);
		} else {
			return A2(elm$core$Dict$remove, targetKey, dictionary);
		}
	});
var elm$core$Maybe$isJust = function (maybe) {
	if (!maybe.$) {
		return true;
	} else {
		return false;
	}
};
var elm$core$Result$map = F2(
	function (func, ra) {
		if (!ra.$) {
			var a = ra.a;
			return elm$core$Result$Ok(
				func(a));
		} else {
			var e = ra.a;
			return elm$core$Result$Err(e);
		}
	});
var elm$http$Http$BadPayload = F2(
	function (a, b) {
		return {$: 4, a: a, b: b};
	});
var elm$http$Http$BadStatus = function (a) {
	return {$: 3, a: a};
};
var elm$http$Http$BadUrl = function (a) {
	return {$: 0, a: a};
};
var elm$http$Http$NetworkError = {$: 2};
var elm$http$Http$Timeout = {$: 1};
var elm$http$Http$Internal$FormDataBody = function (a) {
	return {$: 2, a: a};
};
var elm$http$Http$Internal$isStringBody = function (body) {
	if (body.$ === 1) {
		return true;
	} else {
		return false;
	}
};
var elm$http$Http$expectStringResponse = _Http_expectStringResponse;
var elm$http$Http$expectString = elm$http$Http$expectStringResponse(
	function (response) {
		return elm$core$Result$Ok(response.f);
	});
var elm$http$Http$Internal$Request = elm$core$Basics$identity;
var elm$http$Http$request = elm$core$Basics$identity;
var elm$http$Http$getString = function (url) {
	return elm$http$Http$request(
		{f: elm$http$Http$emptyBody, j: elm$http$Http$expectString, k: _List_Nil, l: 'GET', m: elm$core$Maybe$Nothing, n: url, o: false});
};
var elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var elm$core$Task$onError = _Scheduler_onError;
var elm$core$Task$attempt = F2(
	function (resultToMessage, task) {
		return elm$core$Task$command(
			A2(
				elm$core$Task$onError,
				A2(
					elm$core$Basics$composeL,
					A2(elm$core$Basics$composeL, elm$core$Task$succeed, resultToMessage),
					elm$core$Result$Err),
				A2(
					elm$core$Task$andThen,
					A2(
						elm$core$Basics$composeL,
						A2(elm$core$Basics$composeL, elm$core$Task$succeed, resultToMessage),
						elm$core$Result$Ok),
					task)));
	});
var elm$http$Http$toTask = function (_n0) {
	var request_ = _n0;
	return A2(_Http_toTask, request_, elm$core$Maybe$Nothing);
};
var elm$http$Http$send = F2(
	function (resultToMessage, request_) {
		return A2(
			elm$core$Task$attempt,
			resultToMessage,
			elm$http$Http$toTask(request_));
	});
var author$project$Page$ApiDocument$getWebApiDocument = A2(
	elm$http$Http$send,
	author$project$Page$ApiDocument$UpdateWebApiDocument,
	elm$http$Http$getString('public/WebApiDocument.md'));
var author$project$Page$ApiDocument$init = function (session) {
	var model = {v: false, ac: session, ag: elm$core$Maybe$Nothing};
	return _Utils_Tuple2(model, author$project$Page$ApiDocument$getWebApiDocument);
};
var author$project$Page$Charts$init = function (session) {
	var model = {v: false, ac: session};
	return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
};
var author$project$Api$Endpoint$SystemHealth = function (system) {
	return {cp: system};
};
var author$project$Api$Endpoint$decodeSystemSignal = elm$json$Json$Decode$string;
var author$project$Api$Endpoint$decodeSystemHealth = A3(
	NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
	'system',
	author$project$Api$Endpoint$decodeSystemSignal,
	elm$json$Json$Decode$succeed(author$project$Api$Endpoint$SystemHealth));
var elm$http$Http$expectJson = function (decoder) {
	return elm$http$Http$expectStringResponse(
		function (response) {
			var _n0 = A2(elm$json$Json$Decode$decodeString, decoder, response.f);
			if (_n0.$ === 1) {
				var decodeError = _n0.a;
				return elm$core$Result$Err(
					elm$json$Json$Decode$errorToString(decodeError));
			} else {
				var value = _n0.a;
				return elm$core$Result$Ok(value);
			}
		});
};
var author$project$Api$Endpoint$getApiV1Health = elm$http$Http$request(
	{
		f: elm$http$Http$emptyBody,
		j: elm$http$Http$expectJson(author$project$Api$Endpoint$decodeSystemHealth),
		k: _List_Nil,
		l: 'GET',
		m: elm$core$Maybe$Nothing,
		n: A2(
			elm$core$String$join,
			'/',
			_List_fromArray(
				['https://tractor.ak1211.com', 'api', 'v1', 'health'])),
		o: false
	});
var author$project$Page$Dashboard$GotSystemHealth = function (a) {
	return {$: 3, a: a};
};
var author$project$Page$Dashboard$getSystemHealth = A2(elm$http$Http$send, author$project$Page$Dashboard$GotSystemHealth, author$project$Api$Endpoint$getApiV1Health);
var author$project$Api$Endpoint$VerRev = F8(
	function (version, buildArch, buildOS, gitBranch, gitHash, gitCommitDate, gitCommitCount, gitStatus) {
		return {aR: buildArch, aS: buildOS, a4: gitBranch, a5: gitCommitCount, a6: gitCommitDate, a7: gitHash, a8: gitStatus, bO: version};
	});
var author$project$Api$Endpoint$decodeVerRev = A3(
	NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
	'gitStatus',
	elm$json$Json$Decode$string,
	A3(
		NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
		'gitCommitCount',
		elm$json$Json$Decode$string,
		A3(
			NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
			'gitCommitDate',
			elm$json$Json$Decode$string,
			A3(
				NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
				'gitHash',
				elm$json$Json$Decode$string,
				A3(
					NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
					'gitBranch',
					elm$json$Json$Decode$string,
					A3(
						NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
						'buildOS',
						elm$json$Json$Decode$string,
						A3(
							NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
							'buildArch',
							elm$json$Json$Decode$string,
							A3(
								NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
								'version',
								elm$json$Json$Decode$string,
								elm$json$Json$Decode$succeed(author$project$Api$Endpoint$VerRev)))))))));
var author$project$Api$Endpoint$getApiV1Version = elm$http$Http$request(
	{
		f: elm$http$Http$emptyBody,
		j: elm$http$Http$expectJson(author$project$Api$Endpoint$decodeVerRev),
		k: _List_Nil,
		l: 'GET',
		m: elm$core$Maybe$Nothing,
		n: A2(
			elm$core$String$join,
			'/',
			_List_fromArray(
				['https://tractor.ak1211.com', 'api', 'v1', 'version'])),
		o: false
	});
var author$project$Page$Dashboard$GotSystemVersion = function (a) {
	return {$: 4, a: a};
};
var author$project$Page$Dashboard$getSystemVersion = A2(elm$http$Http$send, author$project$Page$Dashboard$GotSystemVersion, author$project$Api$Endpoint$getApiV1Version);
var author$project$Page$Dashboard$init = function (session) {
	var model = {v: false, ac: session, ad: elm$core$Maybe$Nothing, ae: elm$core$Maybe$Nothing};
	return _Utils_Tuple2(
		model,
		elm$core$Platform$Cmd$batch(
			_List_fromArray(
				[author$project$Page$Dashboard$getSystemHealth, author$project$Page$Dashboard$getSystemVersion])));
};
var author$project$Page$Login$Error = function (a) {
	return {$: 2, a: a};
};
var author$project$Page$Login$First = function (a) {
	return {$: 0, a: a};
};
var author$project$Page$Login$Redirected = function (a) {
	return {$: 1, a: a};
};
var author$project$Api$Endpoint$AuthClientId = function (clientid) {
	return {aW: clientid};
};
var author$project$Api$Endpoint$decodeAuthClientId = A3(
	NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
	'clientid',
	elm$json$Json$Decode$string,
	elm$json$Json$Decode$succeed(author$project$Api$Endpoint$AuthClientId));
var author$project$Api$Endpoint$getApiV1AuthClientid = elm$http$Http$request(
	{
		f: elm$http$Http$emptyBody,
		j: elm$http$Http$expectJson(author$project$Api$Endpoint$decodeAuthClientId),
		k: _List_Nil,
		l: 'GET',
		m: elm$core$Maybe$Nothing,
		n: A2(
			elm$core$String$join,
			'/',
			_List_fromArray(
				['https://tractor.ak1211.com', 'api', 'v1', 'auth', 'clientid'])),
		o: false
	});
var author$project$Api$Endpoint$RespAuth = F3(
	function (accessToken, expiresIn, tokenType) {
		return {aN: accessToken, a2: expiresIn, bH: tokenType};
	});
var elm$json$Json$Decode$int = _Json_decodeInt;
var author$project$Api$Endpoint$decodeRespAuth = A3(
	NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
	'tokenType',
	elm$json$Json$Decode$string,
	A3(
		NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
		'expiresIn',
		elm$json$Json$Decode$int,
		A3(
			NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
			'accessToken',
			elm$json$Json$Decode$string,
			elm$json$Json$Decode$succeed(author$project$Api$Endpoint$RespAuth))));
var elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _n0 = f(mx);
		if (!_n0.$) {
			var x = _n0.a;
			return A2(elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			elm$core$List$foldr,
			elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return elm$core$Maybe$Just(
				f(value));
		} else {
			return elm$core$Maybe$Nothing;
		}
	});
var elm$http$Http$Internal$Header = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var elm$http$Http$header = elm$http$Http$Internal$Header;
var author$project$Api$Endpoint$getApiV1Token = function (header_Authorization) {
	return elm$http$Http$request(
		{
			f: elm$http$Http$emptyBody,
			j: elm$http$Http$expectJson(author$project$Api$Endpoint$decodeRespAuth),
			k: A2(
				elm$core$List$filterMap,
				elm$core$Basics$identity,
				_List_fromArray(
					[
						A2(
						elm$core$Maybe$map,
						elm$http$Http$header('Authorization'),
						elm$core$Maybe$Just(header_Authorization))
					])),
			l: 'GET',
			m: elm$core$Maybe$Nothing,
			n: A2(
				elm$core$String$join,
				'/',
				_List_fromArray(
					['https://tractor.ak1211.com', 'api', 'v1', 'token'])),
			o: false
		});
};
var author$project$Page$Login$GotAccessToken = function (a) {
	return {$: 3, a: a};
};
var elm$core$String$foldl = _String_foldl;
var elm$core$Basics$ge = _Utils_ge;
var elm$core$Bitwise$and = _Bitwise_and;
var elm$core$Bitwise$or = _Bitwise_or;
var elm$core$Bitwise$shiftRightZfBy = _Bitwise_shiftRightZfBy;
var elm$core$Bitwise$shiftLeftBy = _Bitwise_shiftLeftBy;
var truqu$elm_base64$Base64$Encode$intToBase64 = function (i) {
	switch (i) {
		case 0:
			return 'A';
		case 1:
			return 'B';
		case 2:
			return 'C';
		case 3:
			return 'D';
		case 4:
			return 'E';
		case 5:
			return 'F';
		case 6:
			return 'G';
		case 7:
			return 'H';
		case 8:
			return 'I';
		case 9:
			return 'J';
		case 10:
			return 'K';
		case 11:
			return 'L';
		case 12:
			return 'M';
		case 13:
			return 'N';
		case 14:
			return 'O';
		case 15:
			return 'P';
		case 16:
			return 'Q';
		case 17:
			return 'R';
		case 18:
			return 'S';
		case 19:
			return 'T';
		case 20:
			return 'U';
		case 21:
			return 'V';
		case 22:
			return 'W';
		case 23:
			return 'X';
		case 24:
			return 'Y';
		case 25:
			return 'Z';
		case 26:
			return 'a';
		case 27:
			return 'b';
		case 28:
			return 'c';
		case 29:
			return 'd';
		case 30:
			return 'e';
		case 31:
			return 'f';
		case 32:
			return 'g';
		case 33:
			return 'h';
		case 34:
			return 'i';
		case 35:
			return 'j';
		case 36:
			return 'k';
		case 37:
			return 'l';
		case 38:
			return 'm';
		case 39:
			return 'n';
		case 40:
			return 'o';
		case 41:
			return 'p';
		case 42:
			return 'q';
		case 43:
			return 'r';
		case 44:
			return 's';
		case 45:
			return 't';
		case 46:
			return 'u';
		case 47:
			return 'v';
		case 48:
			return 'w';
		case 49:
			return 'x';
		case 50:
			return 'y';
		case 51:
			return 'z';
		case 52:
			return '0';
		case 53:
			return '1';
		case 54:
			return '2';
		case 55:
			return '3';
		case 56:
			return '4';
		case 57:
			return '5';
		case 58:
			return '6';
		case 59:
			return '7';
		case 60:
			return '8';
		case 61:
			return '9';
		case 62:
			return '+';
		default:
			return '/';
	}
};
var truqu$elm_base64$Base64$Encode$toBase64 = function (_int) {
	return _Utils_ap(
		truqu$elm_base64$Base64$Encode$intToBase64(63 & (_int >>> 18)),
		_Utils_ap(
			truqu$elm_base64$Base64$Encode$intToBase64(63 & (_int >>> 12)),
			_Utils_ap(
				truqu$elm_base64$Base64$Encode$intToBase64(63 & (_int >>> 6)),
				truqu$elm_base64$Base64$Encode$intToBase64(63 & (_int >>> 0)))));
};
var truqu$elm_base64$Base64$Encode$add = F2(
	function (_char, _n0) {
		var res = _n0.a;
		var count = _n0.b;
		var acc = _n0.c;
		var current = (acc << 8) | _char;
		if (count === 2) {
			return _Utils_Tuple3(
				_Utils_ap(
					res,
					truqu$elm_base64$Base64$Encode$toBase64(current)),
				0,
				0);
		} else {
			return _Utils_Tuple3(res, count + 1, current);
		}
	});
var truqu$elm_base64$Base64$Encode$chomp = F2(
	function (char_, acc) {
		var _char = elm$core$Char$toCode(char_);
		return (_char < 128) ? A2(truqu$elm_base64$Base64$Encode$add, _char, acc) : ((_char < 2048) ? A2(
			truqu$elm_base64$Base64$Encode$add,
			128 | (63 & _char),
			A2(truqu$elm_base64$Base64$Encode$add, 192 | (_char >>> 6), acc)) : (((_char < 55296) || ((_char >= 57344) && (_char <= 65535))) ? A2(
			truqu$elm_base64$Base64$Encode$add,
			128 | (63 & _char),
			A2(
				truqu$elm_base64$Base64$Encode$add,
				128 | (63 & (_char >>> 6)),
				A2(truqu$elm_base64$Base64$Encode$add, 224 | (_char >>> 12), acc))) : A2(
			truqu$elm_base64$Base64$Encode$add,
			128 | (63 & _char),
			A2(
				truqu$elm_base64$Base64$Encode$add,
				128 | (63 & (_char >>> 6)),
				A2(
					truqu$elm_base64$Base64$Encode$add,
					128 | (63 & (_char >>> 12)),
					A2(truqu$elm_base64$Base64$Encode$add, 240 | (_char >>> 18), acc))))));
	});
var truqu$elm_base64$Base64$Encode$initial = _Utils_Tuple3('', 0, 0);
var truqu$elm_base64$Base64$Encode$wrapUp = function (_n0) {
	var res = _n0.a;
	var cnt = _n0.b;
	var acc = _n0.c;
	switch (cnt) {
		case 1:
			return res + (truqu$elm_base64$Base64$Encode$intToBase64(63 & (acc >>> 2)) + (truqu$elm_base64$Base64$Encode$intToBase64(63 & (acc << 4)) + '=='));
		case 2:
			return res + (truqu$elm_base64$Base64$Encode$intToBase64(63 & (acc >>> 10)) + (truqu$elm_base64$Base64$Encode$intToBase64(63 & (acc >>> 4)) + (truqu$elm_base64$Base64$Encode$intToBase64(63 & (acc << 2)) + '=')));
		default:
			return res;
	}
};
var truqu$elm_base64$Base64$Encode$encode = function (input) {
	return truqu$elm_base64$Base64$Encode$wrapUp(
		A3(elm$core$String$foldl, truqu$elm_base64$Base64$Encode$chomp, truqu$elm_base64$Base64$Encode$initial, input));
};
var truqu$elm_base64$Base64$encode = truqu$elm_base64$Base64$Encode$encode;
var author$project$Page$Login$getAccessToken = function (authTempCode) {
	var clientidTask = elm$http$Http$toTask(author$project$Api$Endpoint$getApiV1AuthClientid);
	var basicAuth = F2(
		function (user, pass) {
			return 'Basic ' + truqu$elm_base64$Base64$encode(user + (':' + pass));
		});
	var getTokenTask = function (x) {
		return elm$http$Http$toTask(
			author$project$Api$Endpoint$getApiV1Token(
				A2(basicAuth, x.aW, authTempCode.Z)));
	};
	return A2(
		elm$core$Task$attempt,
		author$project$Page$Login$GotAccessToken,
		A2(
			elm$core$Task$andThen,
			function (cid) {
				return getTokenTask(cid);
			},
			clientidTask));
};
var author$project$Page$Login$GotAuthClientId = function (a) {
	return {$: 2, a: a};
};
var author$project$Page$Login$getAuthClientId = A2(elm$http$Http$send, author$project$Page$Login$GotAuthClientId, author$project$Api$Endpoint$getApiV1AuthClientid);
var author$project$Page$Login$init = F2(
	function (result, session) {
		var initRedirectedStage = function (param) {
			return author$project$Page$Login$Redirected(
				{bV: param});
		};
		var initModel = function (x) {
			return {v: false, ac: session, J: x};
		};
		var initFirstStage = author$project$Page$Login$First(
			{ai: elm$core$Maybe$Nothing});
		var initErrorStage = function (msg) {
			return author$project$Page$Login$Error(
				{al: msg});
		};
		if (result.$ === 1) {
			return _Utils_Tuple2(
				initModel(initFirstStage),
				author$project$Page$Login$getAuthClientId);
		} else {
			if (!result.a.$) {
				var param = result.a.a;
				return _Utils_Tuple2(
					initModel(
						initRedirectedStage(param)),
					author$project$Page$Login$getAccessToken(param.Z));
			} else {
				var e = result.a.a;
				return _Utils_Tuple2(
					initModel(
						initErrorStage(e)),
					elm$core$Platform$Cmd$none);
			}
		}
	});
var author$project$Page$Portfolio$init = function (session) {
	var model = {v: false, ac: session};
	return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
};
var author$project$Page$Upload$init = function (session) {
	var model = {v: false, ac: session};
	return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
};
var author$project$Route$Dashboard = {$: 1};
var author$project$Route$routeToString = function (page) {
	var pieces = function () {
		switch (page.$) {
			case 1:
				return _List_fromArray(
					['dashboard']);
			case 0:
				return _List_Nil;
			case 2:
				return _List_fromArray(
					['login']);
			case 3:
				return _List_fromArray(
					['logout']);
			case 4:
				return _List_fromArray(
					['upload']);
			case 5:
				return _List_fromArray(
					['portfolio']);
			case 6:
				return _List_fromArray(
					['charts']);
			case 7:
				return _List_fromArray(
					['account-balance']);
			default:
				return _List_fromArray(
					['api-document']);
		}
	}();
	return '#/' + A2(elm$core$String$join, '/', pieces);
};
var elm$browser$Browser$Navigation$replaceUrl = _Browser_replaceUrl;
var author$project$Route$replaceUrl = F2(
	function (key, route) {
		return A2(
			elm$browser$Browser$Navigation$replaceUrl,
			key,
			author$project$Route$routeToString(route));
	});
var author$project$Session$navKey = function (session) {
	if (!session.$) {
		var key = session.a;
		return key;
	} else {
		var key = session.a;
		return key;
	}
};
var elm$browser$Browser$Navigation$reload = _Browser_reload(false);
var author$project$Main$changeRouteTo = F2(
	function (maybeRoute, model) {
		var session = author$project$Main$toSession(model);
		if (maybeRoute.$ === 1) {
			return _Utils_Tuple2(
				author$project$Main$NotFound(session),
				elm$core$Platform$Cmd$none);
		} else {
			switch (maybeRoute.a.$) {
				case 0:
					var _n1 = maybeRoute.a;
					return _Utils_Tuple2(
						author$project$Main$Redirect(session),
						A2(
							author$project$Route$replaceUrl,
							author$project$Session$navKey(
								author$project$Main$toSession(model)),
							author$project$Route$Dashboard));
				case 3:
					var _n2 = maybeRoute.a;
					return _Utils_Tuple2(
						author$project$Main$Redirect(session),
						elm$core$Platform$Cmd$batch(
							_List_fromArray(
								[
									author$project$Api$logout,
									A2(
									author$project$Route$replaceUrl,
									author$project$Session$navKey(
										author$project$Main$toSession(model)),
									author$project$Route$Dashboard),
									elm$browser$Browser$Navigation$reload
								])));
				case 1:
					var _n3 = maybeRoute.a;
					return A4(
						author$project$Main$updateWith,
						author$project$Main$Dashboard,
						author$project$Main$GotDashboardMsg,
						model,
						author$project$Page$Dashboard$init(session));
				case 2:
					var param = maybeRoute.a.a;
					return A4(
						author$project$Main$updateWith,
						author$project$Main$Login,
						author$project$Main$GotLoginMsg,
						model,
						A2(author$project$Page$Login$init, param, session));
				case 4:
					var _n4 = maybeRoute.a;
					return A4(
						author$project$Main$updateWith,
						author$project$Main$Upload,
						author$project$Main$GotUploadMsg,
						model,
						author$project$Page$Upload$init(session));
				case 5:
					var _n5 = maybeRoute.a;
					return A4(
						author$project$Main$updateWith,
						author$project$Main$Portfolio,
						author$project$Main$GotPortfolioMsg,
						model,
						author$project$Page$Portfolio$init(session));
				case 6:
					var _n6 = maybeRoute.a;
					return A4(
						author$project$Main$updateWith,
						author$project$Main$Charts,
						author$project$Main$GotChartsMsg,
						model,
						author$project$Page$Charts$init(session));
				case 7:
					var _n7 = maybeRoute.a;
					return A4(
						author$project$Main$updateWith,
						author$project$Main$AccountBalance,
						author$project$Main$GotAccountBalanceMsg,
						model,
						author$project$Page$AccountBalance$init(session));
				default:
					var _n8 = maybeRoute.a;
					return A4(
						author$project$Main$updateWith,
						author$project$Main$ApiDocument,
						author$project$Main$GotApiDocumentMsg,
						model,
						author$project$Page$ApiDocument$init(session));
			}
		}
	});
var author$project$Api$Endpoint$AuthTempCode = function (code) {
	return {Z: code};
};
var author$project$Route$AccountBalance = {$: 7};
var author$project$Route$ApiDocument = {$: 8};
var author$project$Route$Charts = {$: 6};
var author$project$Route$Login = function (a) {
	return {$: 2, a: a};
};
var author$project$Route$Logout = {$: 3};
var author$project$Route$Portfolio = {$: 5};
var author$project$Route$Root = {$: 0};
var author$project$Route$Upload = {$: 4};
var elm$url$Url$Parser$Parser = elm$core$Basics$identity;
var elm$url$Url$Parser$State = F5(
	function (visited, unvisited, params, frag, value) {
		return {u: frag, x: params, t: unvisited, r: value, y: visited};
	});
var elm$url$Url$Parser$mapState = F2(
	function (func, _n0) {
		var visited = _n0.y;
		var unvisited = _n0.t;
		var params = _n0.x;
		var frag = _n0.u;
		var value = _n0.r;
		return A5(
			elm$url$Url$Parser$State,
			visited,
			unvisited,
			params,
			frag,
			func(value));
	});
var elm$url$Url$Parser$map = F2(
	function (subValue, _n0) {
		var parseArg = _n0;
		return function (_n1) {
			var visited = _n1.y;
			var unvisited = _n1.t;
			var params = _n1.x;
			var frag = _n1.u;
			var value = _n1.r;
			return A2(
				elm$core$List$map,
				elm$url$Url$Parser$mapState(value),
				parseArg(
					A5(elm$url$Url$Parser$State, visited, unvisited, params, frag, subValue)));
		};
	});
var elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3(elm$core$List$foldr, elm$core$List$cons, ys, xs);
		}
	});
var elm$core$List$concat = function (lists) {
	return A3(elm$core$List$foldr, elm$core$List$append, _List_Nil, lists);
};
var elm$core$List$concatMap = F2(
	function (f, list) {
		return elm$core$List$concat(
			A2(elm$core$List$map, f, list));
	});
var elm$url$Url$Parser$oneOf = function (parsers) {
	return function (state) {
		return A2(
			elm$core$List$concatMap,
			function (_n0) {
				var parser = _n0;
				return parser(state);
			},
			parsers);
	};
};
var elm$url$Url$Parser$query = function (_n0) {
	var queryParser = _n0;
	return function (_n1) {
		var visited = _n1.y;
		var unvisited = _n1.t;
		var params = _n1.x;
		var frag = _n1.u;
		var value = _n1.r;
		return _List_fromArray(
			[
				A5(
				elm$url$Url$Parser$State,
				visited,
				unvisited,
				params,
				frag,
				value(
					queryParser(params)))
			]);
	};
};
var elm$url$Url$Parser$slash = F2(
	function (_n0, _n1) {
		var parseBefore = _n0;
		var parseAfter = _n1;
		return function (state) {
			return A2(
				elm$core$List$concatMap,
				parseAfter,
				parseBefore(state));
		};
	});
var elm$url$Url$Parser$questionMark = F2(
	function (parser, queryParser) {
		return A2(
			elm$url$Url$Parser$slash,
			parser,
			elm$url$Url$Parser$query(queryParser));
	});
var elm$url$Url$Parser$s = function (str) {
	return function (_n0) {
		var visited = _n0.y;
		var unvisited = _n0.t;
		var params = _n0.x;
		var frag = _n0.u;
		var value = _n0.r;
		if (!unvisited.b) {
			return _List_Nil;
		} else {
			var next = unvisited.a;
			var rest = unvisited.b;
			return _Utils_eq(next, str) ? _List_fromArray(
				[
					A5(
					elm$url$Url$Parser$State,
					A2(elm$core$List$cons, next, visited),
					rest,
					params,
					frag,
					value)
				]) : _List_Nil;
		}
	};
};
var elm$url$Url$Parser$top = function (state) {
	return _List_fromArray(
		[state]);
};
var elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var elm$url$Url$Parser$Internal$Parser = elm$core$Basics$identity;
var elm$url$Url$Parser$Query$custom = F2(
	function (key, func) {
		return function (dict) {
			return func(
				A2(
					elm$core$Maybe$withDefault,
					_List_Nil,
					A2(elm$core$Dict$get, key, dict)));
		};
	});
var elm$url$Url$Parser$Query$string = function (key) {
	return A2(
		elm$url$Url$Parser$Query$custom,
		key,
		function (stringList) {
			if (stringList.b && (!stringList.b.b)) {
				var str = stringList.a;
				return elm$core$Maybe$Just(str);
			} else {
				return elm$core$Maybe$Nothing;
			}
		});
};
var author$project$Route$parser = function () {
	var handleRoot = F3(
		function (err, code, state) {
			var _n0 = _Utils_Tuple3(err, code, state);
			if (!_n0.a.$) {
				var e = _n0.a.a;
				var result = elm$core$Result$Err(e);
				return author$project$Route$Login(
					elm$core$Maybe$Just(result));
			} else {
				if ((!_n0.b.$) && (!_n0.c.$)) {
					var _n1 = _n0.a;
					var c = _n0.b.a;
					var s = _n0.c.a;
					var result = elm$core$Result$Ok(
						{
							Z: author$project$Api$Endpoint$AuthTempCode(c),
							bC: s
						});
					return author$project$Route$Login(
						elm$core$Maybe$Just(result));
				} else {
					return author$project$Route$Root;
				}
			}
		});
	return elm$url$Url$Parser$oneOf(
		_List_fromArray(
			[
				A2(
				elm$url$Url$Parser$map,
				handleRoot,
				A2(
					elm$url$Url$Parser$questionMark,
					A2(
						elm$url$Url$Parser$questionMark,
						A2(
							elm$url$Url$Parser$questionMark,
							elm$url$Url$Parser$top,
							elm$url$Url$Parser$Query$string('error')),
						elm$url$Url$Parser$Query$string('code')),
					elm$url$Url$Parser$Query$string('state'))),
				A2(
				elm$url$Url$Parser$map,
				author$project$Route$Dashboard,
				elm$url$Url$Parser$s('dashboard')),
				A2(
				elm$url$Url$Parser$map,
				author$project$Route$Login(elm$core$Maybe$Nothing),
				elm$url$Url$Parser$s('login')),
				A2(
				elm$url$Url$Parser$map,
				author$project$Route$Logout,
				elm$url$Url$Parser$s('logout')),
				A2(
				elm$url$Url$Parser$map,
				author$project$Route$Upload,
				elm$url$Url$Parser$s('upload')),
				A2(
				elm$url$Url$Parser$map,
				author$project$Route$Portfolio,
				elm$url$Url$Parser$s('portfolio')),
				A2(
				elm$url$Url$Parser$map,
				author$project$Route$Charts,
				elm$url$Url$Parser$s('charts')),
				A2(
				elm$url$Url$Parser$map,
				author$project$Route$AccountBalance,
				elm$url$Url$Parser$s('account-balance')),
				A2(
				elm$url$Url$Parser$map,
				author$project$Route$ApiDocument,
				elm$url$Url$Parser$s('api-document'))
			]));
}();
var elm$url$Url$Parser$getFirstMatch = function (states) {
	getFirstMatch:
	while (true) {
		if (!states.b) {
			return elm$core$Maybe$Nothing;
		} else {
			var state = states.a;
			var rest = states.b;
			var _n1 = state.t;
			if (!_n1.b) {
				return elm$core$Maybe$Just(state.r);
			} else {
				if ((_n1.a === '') && (!_n1.b.b)) {
					return elm$core$Maybe$Just(state.r);
				} else {
					var $temp$states = rest;
					states = $temp$states;
					continue getFirstMatch;
				}
			}
		}
	}
};
var elm$url$Url$Parser$removeFinalEmpty = function (segments) {
	if (!segments.b) {
		return _List_Nil;
	} else {
		if ((segments.a === '') && (!segments.b.b)) {
			return _List_Nil;
		} else {
			var segment = segments.a;
			var rest = segments.b;
			return A2(
				elm$core$List$cons,
				segment,
				elm$url$Url$Parser$removeFinalEmpty(rest));
		}
	}
};
var elm$url$Url$Parser$preparePath = function (path) {
	var _n0 = A2(elm$core$String$split, '/', path);
	if (_n0.b && (_n0.a === '')) {
		var segments = _n0.b;
		return elm$url$Url$Parser$removeFinalEmpty(segments);
	} else {
		var segments = _n0;
		return elm$url$Url$Parser$removeFinalEmpty(segments);
	}
};
var elm$url$Url$percentDecode = _Url_percentDecode;
var elm$url$Url$Parser$addToParametersHelp = F2(
	function (value, maybeList) {
		if (maybeList.$ === 1) {
			return elm$core$Maybe$Just(
				_List_fromArray(
					[value]));
		} else {
			var list = maybeList.a;
			return elm$core$Maybe$Just(
				A2(elm$core$List$cons, value, list));
		}
	});
var elm$url$Url$Parser$addParam = F2(
	function (segment, dict) {
		var _n0 = A2(elm$core$String$split, '=', segment);
		if ((_n0.b && _n0.b.b) && (!_n0.b.b.b)) {
			var rawKey = _n0.a;
			var _n1 = _n0.b;
			var rawValue = _n1.a;
			var _n2 = elm$url$Url$percentDecode(rawKey);
			if (_n2.$ === 1) {
				return dict;
			} else {
				var key = _n2.a;
				var _n3 = elm$url$Url$percentDecode(rawValue);
				if (_n3.$ === 1) {
					return dict;
				} else {
					var value = _n3.a;
					return A3(
						elm$core$Dict$update,
						key,
						elm$url$Url$Parser$addToParametersHelp(value),
						dict);
				}
			}
		} else {
			return dict;
		}
	});
var elm$url$Url$Parser$prepareQuery = function (maybeQuery) {
	if (maybeQuery.$ === 1) {
		return elm$core$Dict$empty;
	} else {
		var qry = maybeQuery.a;
		return A3(
			elm$core$List$foldr,
			elm$url$Url$Parser$addParam,
			elm$core$Dict$empty,
			A2(elm$core$String$split, '&', qry));
	}
};
var elm$url$Url$Parser$parse = F2(
	function (_n0, url) {
		var parser = _n0;
		return elm$url$Url$Parser$getFirstMatch(
			parser(
				A5(
					elm$url$Url$Parser$State,
					_List_Nil,
					elm$url$Url$Parser$preparePath(url.ck),
					elm$url$Url$Parser$prepareQuery(url.bu),
					url.b8,
					elm$core$Basics$identity)));
	});
var author$project$Route$fromUrl = function (url) {
	return A2(
		elm$url$Url$Parser$parse,
		author$project$Route$parser,
		_Utils_update(
			url,
			{
				b8: elm$core$Maybe$Nothing,
				ck: A2(elm$core$Maybe$withDefault, '', url.b8)
			}));
};
var author$project$Session$Guest = function (a) {
	return {$: 1, a: a};
};
var author$project$Session$LoggedIn = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var author$project$Session$fromViewer = F2(
	function (key, maybeViewer) {
		if (!maybeViewer.$) {
			var viewerVal = maybeViewer.a;
			return A2(author$project$Session$LoggedIn, key, viewerVal);
		} else {
			return author$project$Session$Guest(key);
		}
	});
var author$project$Main$init = F3(
	function (maybeViewer, url, navKey) {
		return A2(
			author$project$Main$changeRouteTo,
			author$project$Route$fromUrl(url),
			author$project$Main$Redirect(
				A2(author$project$Session$fromViewer, navKey, maybeViewer)));
	});
var author$project$Main$GotSession = function (a) {
	return {$: 11, a: a};
};
var elm$core$Platform$Sub$batch = _Platform_batch;
var elm$core$Platform$Sub$none = elm$core$Platform$Sub$batch(_List_Nil);
var author$project$Page$AccountBalance$subscriptions = function (model) {
	return elm$core$Platform$Sub$none;
};
var author$project$Page$ApiDocument$subscriptions = function (model) {
	return elm$core$Platform$Sub$none;
};
var author$project$Page$Charts$subscriptions = function (model) {
	return elm$core$Platform$Sub$none;
};
var author$project$Page$Dashboard$subscriptions = function (model) {
	return elm$core$Platform$Sub$none;
};
var author$project$Page$Login$subscriptions = function (model) {
	return elm$core$Platform$Sub$none;
};
var author$project$Page$Portfolio$subscriptions = function (model) {
	return elm$core$Platform$Sub$none;
};
var author$project$Page$Upload$subscriptions = function (model) {
	return elm$core$Platform$Sub$none;
};
var author$project$Api$decodeFromChange = F2(
	function (viewerDecoder, val) {
		return elm$core$Result$toMaybe(
			A2(
				elm$json$Json$Decode$decodeValue,
				author$project$Api$storageDecoder(viewerDecoder),
				val));
	});
var elm$json$Json$Decode$value = _Json_decodeValue;
var author$project$Api$onStoreChange = _Platform_incomingPort('onStoreChange', elm$json$Json$Decode$value);
var author$project$Api$viewerChanges = F2(
	function (toMsg, decoder) {
		return author$project$Api$onStoreChange(
			function (value) {
				return toMsg(
					A2(author$project$Api$decodeFromChange, decoder, value));
			});
	});
var author$project$Viewer$Viewer = elm$core$Basics$identity;
var author$project$Viewer$decoder = elm$json$Json$Decode$succeed(elm$core$Basics$identity);
var author$project$Session$changes = F2(
	function (toMsg, key) {
		return A2(
			author$project$Api$viewerChanges,
			function (maybeViewer) {
				return toMsg(
					A2(author$project$Session$fromViewer, key, maybeViewer));
			},
			author$project$Viewer$decoder);
	});
var elm$core$Platform$Sub$map = _Platform_map;
var author$project$Main$subscriptions = function (model) {
	switch (model.$) {
		case 1:
			return elm$core$Platform$Sub$none;
		case 0:
			return A2(
				author$project$Session$changes,
				author$project$Main$GotSession,
				author$project$Session$navKey(
					author$project$Main$toSession(model)));
		case 2:
			var dashboard = model.a;
			return A2(
				elm$core$Platform$Sub$map,
				author$project$Main$GotDashboardMsg,
				author$project$Page$Dashboard$subscriptions(dashboard));
		case 7:
			var login = model.a;
			return A2(
				elm$core$Platform$Sub$map,
				author$project$Main$GotLoginMsg,
				author$project$Page$Login$subscriptions(login));
		case 3:
			var upload = model.a;
			return A2(
				elm$core$Platform$Sub$map,
				author$project$Main$GotUploadMsg,
				author$project$Page$Upload$subscriptions(upload));
		case 4:
			var portfolio = model.a;
			return A2(
				elm$core$Platform$Sub$map,
				author$project$Main$GotPortfolioMsg,
				author$project$Page$Portfolio$subscriptions(portfolio));
		case 5:
			var charts = model.a;
			return A2(
				elm$core$Platform$Sub$map,
				author$project$Main$GotChartsMsg,
				author$project$Page$Charts$subscriptions(charts));
		case 6:
			var accbalance = model.a;
			return A2(
				elm$core$Platform$Sub$map,
				author$project$Main$GotAccountBalanceMsg,
				author$project$Page$AccountBalance$subscriptions(accbalance));
		default:
			var apidocument = model.a;
			return A2(
				elm$core$Platform$Sub$map,
				author$project$Main$GotApiDocumentMsg,
				author$project$Page$ApiDocument$subscriptions(apidocument));
	}
};
var elm$core$Basics$not = _Basics_not;
var author$project$Page$AccountBalance$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 0:
				return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
			case 1:
				var session = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{ac: session}),
					elm$core$Platform$Cmd$none);
			default:
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{v: !model.v}),
					elm$core$Platform$Cmd$none);
		}
	});
var author$project$Page$ApiDocument$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 0:
				return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
			case 1:
				var session = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{ac: session}),
					elm$core$Platform$Cmd$none);
			case 2:
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{v: !model.v}),
					elm$core$Platform$Cmd$none);
			default:
				var result = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							ag: elm$core$Result$toMaybe(result)
						}),
					elm$core$Platform$Cmd$none);
		}
	});
var author$project$Page$Charts$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 0:
				return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
			case 1:
				var session = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{ac: session}),
					elm$core$Platform$Cmd$none);
			default:
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{v: !model.v}),
					elm$core$Platform$Cmd$none);
		}
	});
var author$project$Page$Dashboard$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 0:
				return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
			case 1:
				var session = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{ac: session}),
					elm$core$Platform$Cmd$none);
			case 2:
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{v: !model.v}),
					elm$core$Platform$Cmd$none);
			case 3:
				var result = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							ad: elm$core$Maybe$Just(result)
						}),
					elm$core$Platform$Cmd$none);
			default:
				var result = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							ae: elm$core$Result$toMaybe(result)
						}),
					elm$core$Platform$Cmd$none);
		}
	});
var author$project$Api$JWTContents = function (dat) {
	return {aZ: dat};
};
var author$project$Api$Endpoint$AuthenticatedUser = F5(
	function (userId, userName, userRealName, userTz, userTzOffset) {
		return {bK: userId, bL: userName, cs: userRealName, bM: userTz, bN: userTzOffset};
	});
var author$project$Api$Endpoint$decodeAuthenticatedUser = A3(
	NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
	'userTzOffset',
	elm$json$Json$Decode$int,
	A3(
		NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
		'userTz',
		elm$json$Json$Decode$string,
		A3(
			NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
			'userRealName',
			elm$json$Json$Decode$string,
			A3(
				NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
				'userName',
				elm$json$Json$Decode$string,
				A3(
					NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
					'userId',
					elm$json$Json$Decode$string,
					elm$json$Json$Decode$succeed(author$project$Api$Endpoint$AuthenticatedUser))))));
var author$project$Api$decodeJWTContents = A3(
	NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
	'dat',
	author$project$Api$Endpoint$decodeAuthenticatedUser,
	elm$json$Json$Decode$succeed(author$project$Api$JWTContents));
var elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var elm$core$Result$mapError = F2(
	function (f, result) {
		if (!result.$) {
			var v = result.a;
			return elm$core$Result$Ok(v);
		} else {
			var e = result.a;
			return elm$core$Result$Err(
				f(e));
		}
	});
var simonh1000$elm_jwt$Jwt$TokenDecodeError = function (a) {
	return {$: 4, a: a};
};
var simonh1000$elm_jwt$Jwt$TokenProcessingError = function (a) {
	return {$: 3, a: a};
};
var elm$core$Basics$modBy = _Basics_modBy;
var elm$core$String$concat = function (strings) {
	return A2(elm$core$String$join, '', strings);
};
var simonh1000$elm_jwt$Jwt$fixlength = function (s) {
	var _n0 = A2(
		elm$core$Basics$modBy,
		4,
		elm$core$String$length(s));
	switch (_n0) {
		case 0:
			return elm$core$Result$Ok(s);
		case 2:
			return elm$core$Result$Ok(
				elm$core$String$concat(
					_List_fromArray(
						[s, '=='])));
		case 3:
			return elm$core$Result$Ok(
				elm$core$String$concat(
					_List_fromArray(
						[s, '='])));
		default:
			return elm$core$Result$Err(
				simonh1000$elm_jwt$Jwt$TokenProcessingError('Wrong length'));
	}
};
var elm$core$String$map = _String_map;
var simonh1000$elm_jwt$Jwt$unurl = function () {
	var fix = function (c) {
		switch (c) {
			case '-':
				return '+';
			case '_':
				return '/';
			default:
				return c;
		}
	};
	return elm$core$String$map(fix);
}();
var simonh1000$elm_jwt$Jwt$getTokenBody = function (token) {
	var processor = A2(
		elm$core$Basics$composeR,
		simonh1000$elm_jwt$Jwt$unurl,
		A2(
			elm$core$Basics$composeR,
			elm$core$String$split('.'),
			elm$core$List$map(simonh1000$elm_jwt$Jwt$fixlength)));
	var _n0 = processor(token);
	_n0$2:
	while (true) {
		if (_n0.b && _n0.b.b) {
			if (_n0.b.a.$ === 1) {
				if (_n0.b.b.b && (!_n0.b.b.b.b)) {
					var _n1 = _n0.b;
					var e = _n1.a.a;
					var _n2 = _n1.b;
					return elm$core$Result$Err(e);
				} else {
					break _n0$2;
				}
			} else {
				if (_n0.b.b.b && (!_n0.b.b.b.b)) {
					var _n3 = _n0.b;
					var encBody = _n3.a.a;
					var _n4 = _n3.b;
					return elm$core$Result$Ok(encBody);
				} else {
					break _n0$2;
				}
			}
		} else {
			break _n0$2;
		}
	}
	return elm$core$Result$Err(
		simonh1000$elm_jwt$Jwt$TokenProcessingError('Token has invalid shape'));
};
var truqu$elm_base64$Base64$Decode$pad = function (input) {
	var _n0 = elm$core$String$length(input) % 4;
	switch (_n0) {
		case 3:
			return input + '=';
		case 2:
			return input + '==';
		default:
			return input;
	}
};
var truqu$elm_base64$Base64$Decode$charToInt = function (_char) {
	switch (_char) {
		case 'A':
			return 0;
		case 'B':
			return 1;
		case 'C':
			return 2;
		case 'D':
			return 3;
		case 'E':
			return 4;
		case 'F':
			return 5;
		case 'G':
			return 6;
		case 'H':
			return 7;
		case 'I':
			return 8;
		case 'J':
			return 9;
		case 'K':
			return 10;
		case 'L':
			return 11;
		case 'M':
			return 12;
		case 'N':
			return 13;
		case 'O':
			return 14;
		case 'P':
			return 15;
		case 'Q':
			return 16;
		case 'R':
			return 17;
		case 'S':
			return 18;
		case 'T':
			return 19;
		case 'U':
			return 20;
		case 'V':
			return 21;
		case 'W':
			return 22;
		case 'X':
			return 23;
		case 'Y':
			return 24;
		case 'Z':
			return 25;
		case 'a':
			return 26;
		case 'b':
			return 27;
		case 'c':
			return 28;
		case 'd':
			return 29;
		case 'e':
			return 30;
		case 'f':
			return 31;
		case 'g':
			return 32;
		case 'h':
			return 33;
		case 'i':
			return 34;
		case 'j':
			return 35;
		case 'k':
			return 36;
		case 'l':
			return 37;
		case 'm':
			return 38;
		case 'n':
			return 39;
		case 'o':
			return 40;
		case 'p':
			return 41;
		case 'q':
			return 42;
		case 'r':
			return 43;
		case 's':
			return 44;
		case 't':
			return 45;
		case 'u':
			return 46;
		case 'v':
			return 47;
		case 'w':
			return 48;
		case 'x':
			return 49;
		case 'y':
			return 50;
		case 'z':
			return 51;
		case '0':
			return 52;
		case '1':
			return 53;
		case '2':
			return 54;
		case '3':
			return 55;
		case '4':
			return 56;
		case '5':
			return 57;
		case '6':
			return 58;
		case '7':
			return 59;
		case '8':
			return 60;
		case '9':
			return 61;
		case '+':
			return 62;
		case '/':
			return 63;
		default:
			return 0;
	}
};
var elm$core$Char$fromCode = _Char_fromCode;
var elm$core$String$cons = _String_cons;
var elm$core$String$fromChar = function (_char) {
	return A2(elm$core$String$cons, _char, '');
};
var truqu$elm_base64$Base64$Decode$intToString = A2(elm$core$Basics$composeR, elm$core$Char$fromCode, elm$core$String$fromChar);
var truqu$elm_base64$Base64$Decode$add = F2(
	function (_char, _n0) {
		var curr = _n0.a;
		var need = _n0.b;
		var res = _n0.c;
		var shiftAndAdd = function (_int) {
			return (63 & _int) | (curr << 6);
		};
		return (!need) ? ((!(128 & _char)) ? _Utils_Tuple3(
			0,
			0,
			_Utils_ap(
				res,
				truqu$elm_base64$Base64$Decode$intToString(_char))) : (((224 & _char) === 192) ? _Utils_Tuple3(31 & _char, 1, res) : (((240 & _char) === 224) ? _Utils_Tuple3(15 & _char, 2, res) : _Utils_Tuple3(7 & _char, 3, res)))) : ((need === 1) ? _Utils_Tuple3(
			0,
			0,
			_Utils_ap(
				res,
				truqu$elm_base64$Base64$Decode$intToString(
					shiftAndAdd(_char)))) : _Utils_Tuple3(
			shiftAndAdd(_char),
			need - 1,
			res));
	});
var truqu$elm_base64$Base64$Decode$toUTF16 = F2(
	function (_char, acc) {
		return _Utils_Tuple3(
			0,
			0,
			A2(
				truqu$elm_base64$Base64$Decode$add,
				255 & (_char >>> 0),
				A2(
					truqu$elm_base64$Base64$Decode$add,
					255 & (_char >>> 8),
					A2(truqu$elm_base64$Base64$Decode$add, 255 & (_char >>> 16), acc))));
	});
var truqu$elm_base64$Base64$Decode$chomp = F2(
	function (char_, _n0) {
		var curr = _n0.a;
		var cnt = _n0.b;
		var utf8ToUtf16 = _n0.c;
		var _char = truqu$elm_base64$Base64$Decode$charToInt(char_);
		if (cnt === 3) {
			return A2(truqu$elm_base64$Base64$Decode$toUTF16, curr | _char, utf8ToUtf16);
		} else {
			return _Utils_Tuple3((_char << ((3 - cnt) * 6)) | curr, cnt + 1, utf8ToUtf16);
		}
	});
var truqu$elm_base64$Base64$Decode$initial = _Utils_Tuple3(
	0,
	0,
	_Utils_Tuple3(0, 0, ''));
var elm$core$Basics$negate = function (n) {
	return -n;
};
var elm$core$String$dropRight = F2(
	function (n, string) {
		return (n < 1) ? string : A3(elm$core$String$slice, 0, -n, string);
	});
var elm$core$String$endsWith = _String_endsWith;
var truqu$elm_base64$Base64$Decode$stripNulls = F2(
	function (input, output) {
		return A2(elm$core$String$endsWith, '==', input) ? A2(elm$core$String$dropRight, 2, output) : (A2(elm$core$String$endsWith, '=', input) ? A2(elm$core$String$dropRight, 1, output) : output);
	});
var elm$regex$Regex$Match = F4(
	function (match, index, number, submatches) {
		return {ce: index, cf: match, ci: number, co: submatches};
	});
var elm$regex$Regex$contains = _Regex_contains;
var elm$regex$Regex$fromStringWith = _Regex_fromStringWith;
var elm$regex$Regex$fromString = function (string) {
	return A2(
		elm$regex$Regex$fromStringWith,
		{aU: false, bj: false},
		string);
};
var elm$regex$Regex$never = _Regex_never;
var truqu$elm_base64$Base64$Decode$validBase64Regex = A2(
	elm$core$Maybe$withDefault,
	elm$regex$Regex$never,
	elm$regex$Regex$fromString('^([A-Za-z0-9\\/+]{4})*([A-Za-z0-9\\/+]{2}[A-Za-z0-9\\/+=]{2})?$'));
var truqu$elm_base64$Base64$Decode$validate = function (input) {
	return A2(elm$regex$Regex$contains, truqu$elm_base64$Base64$Decode$validBase64Regex, input) ? elm$core$Result$Ok(input) : elm$core$Result$Err('Invalid base64');
};
var truqu$elm_base64$Base64$Decode$wrapUp = function (_n0) {
	var _n1 = _n0.c;
	var need = _n1.b;
	var res = _n1.c;
	return (need > 0) ? elm$core$Result$Err('Invalid UTF-16') : elm$core$Result$Ok(res);
};
var truqu$elm_base64$Base64$Decode$validateAndDecode = function (input) {
	return A2(
		elm$core$Result$map,
		truqu$elm_base64$Base64$Decode$stripNulls(input),
		A2(
			elm$core$Result$andThen,
			A2(
				elm$core$Basics$composeR,
				A2(elm$core$String$foldl, truqu$elm_base64$Base64$Decode$chomp, truqu$elm_base64$Base64$Decode$initial),
				truqu$elm_base64$Base64$Decode$wrapUp),
			truqu$elm_base64$Base64$Decode$validate(input)));
};
var truqu$elm_base64$Base64$Decode$decode = A2(elm$core$Basics$composeR, truqu$elm_base64$Base64$Decode$pad, truqu$elm_base64$Base64$Decode$validateAndDecode);
var truqu$elm_base64$Base64$decode = truqu$elm_base64$Base64$Decode$decode;
var simonh1000$elm_jwt$Jwt$decodeToken = function (dec) {
	return A2(
		elm$core$Basics$composeR,
		simonh1000$elm_jwt$Jwt$getTokenBody,
		A2(
			elm$core$Basics$composeR,
			elm$core$Result$andThen(
				A2(
					elm$core$Basics$composeR,
					truqu$elm_base64$Base64$decode,
					elm$core$Result$mapError(simonh1000$elm_jwt$Jwt$TokenProcessingError))),
			elm$core$Result$andThen(
				A2(
					elm$core$Basics$composeR,
					elm$json$Json$Decode$decodeString(dec),
					elm$core$Result$mapError(simonh1000$elm_jwt$Jwt$TokenDecodeError)))));
};
var author$project$Api$authenticatedUser = function (jwt) {
	return A2(
		elm$core$Result$map,
		function (a) {
			return a.aZ;
		},
		A2(simonh1000$elm_jwt$Jwt$decodeToken, author$project$Api$decodeJWTContents, jwt));
};
var author$project$Api$credFromJWT = function (jwt) {
	return A2(
		elm$core$Result$map,
		function (x) {
			return A2(author$project$Api$Cred, x.cs, jwt);
		},
		author$project$Api$authenticatedUser(jwt));
};
var elm$json$Json$Encode$string = _Json_wrap;
var author$project$Username$encode = function (_n0) {
	var username = _n0;
	return elm$json$Json$Encode$string(username);
};
var elm$json$Json$Encode$object = function (pairs) {
	return _Json_wrap(
		A3(
			elm$core$List$foldl,
			F2(
				function (_n0, obj) {
					var k = _n0.a;
					var v = _n0.b;
					return A3(_Json_addField, k, v, obj);
				}),
			_Json_emptyObject(0),
			pairs));
};
var author$project$Api$storeCredWith = function (_n0) {
	var uname = _n0.a;
	var token = _n0.b;
	var json = elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'user',
				elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'username',
							author$project$Username$encode(uname)),
							_Utils_Tuple2(
							'token',
							elm$json$Json$Encode$string(token))
						])))
			]));
	return author$project$Api$storeCache(
		elm$core$Maybe$Just(json));
};
var elm$browser$Browser$Navigation$load = _Browser_load;
var author$project$Page$Login$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 0:
				return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
			case 1:
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{v: !model.v}),
					elm$core$Platform$Cmd$none);
			case 3:
				var result = msg.a;
				if (!result.$) {
					var ok = result.a;
					var store = function () {
						var _n2 = author$project$Api$credFromJWT(ok.aN);
						if (!_n2.$) {
							var cred = _n2.a;
							return author$project$Api$storeCredWith(cred);
						} else {
							return elm$core$Platform$Cmd$none;
						}
					}();
					return _Utils_Tuple2(
						model,
						elm$core$Platform$Cmd$batch(
							_List_fromArray(
								[
									store,
									elm$browser$Browser$Navigation$load('/')
								])));
				} else {
					var err = result.a;
					var cause = function () {
						switch (err.$) {
							case 1:
								return 'auth timeout';
							case 3:
								var resp = err.a;
								return 'AuthError (bad status) ' + resp.cm.cg;
							default:
								return 'Auth Error';
						}
					}();
					var newStage = author$project$Page$Login$Error(
						{al: cause});
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{J: newStage}),
						elm$core$Platform$Cmd$none);
				}
			case 2:
				var result = msg.a;
				var _n4 = model.J;
				if (!_n4.$) {
					var stage = _n4.a;
					var newStage = author$project$Page$Login$First(
						_Utils_update(
							stage,
							{
								ai: elm$core$Result$toMaybe(result)
							}));
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{J: newStage}),
						elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
				}
			default:
				var session = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{ac: session}),
					elm$core$Platform$Cmd$none);
		}
	});
var author$project$Page$Portfolio$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 0:
				return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
			case 1:
				var session = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{ac: session}),
					elm$core$Platform$Cmd$none);
			default:
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{v: !model.v}),
					elm$core$Platform$Cmd$none);
		}
	});
var author$project$Page$Upload$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 0:
				return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
			case 1:
				var session = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{ac: session}),
					elm$core$Platform$Cmd$none);
			default:
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{v: !model.v}),
					elm$core$Platform$Cmd$none);
		}
	});
var elm$browser$Browser$Navigation$pushUrl = _Browser_pushUrl;
var elm$url$Url$addPort = F2(
	function (maybePort, starter) {
		if (maybePort.$ === 1) {
			return starter;
		} else {
			var port_ = maybePort.a;
			return starter + (':' + elm$core$String$fromInt(port_));
		}
	});
var elm$url$Url$addPrefixed = F3(
	function (prefix, maybeSegment, starter) {
		if (maybeSegment.$ === 1) {
			return starter;
		} else {
			var segment = maybeSegment.a;
			return _Utils_ap(
				starter,
				_Utils_ap(prefix, segment));
		}
	});
var elm$url$Url$toString = function (url) {
	var http = function () {
		var _n0 = url.bt;
		if (!_n0) {
			return 'http://';
		} else {
			return 'https://';
		}
	}();
	return A3(
		elm$url$Url$addPrefixed,
		'#',
		url.b8,
		A3(
			elm$url$Url$addPrefixed,
			'?',
			url.bu,
			_Utils_ap(
				A2(
					elm$url$Url$addPort,
					url.bp,
					_Utils_ap(http, url.bc)),
				url.ck)));
};
var author$project$Main$update = F2(
	function (msg, model) {
		var _n0 = _Utils_Tuple2(msg, model);
		_n0$12:
		while (true) {
			switch (_n0.a.$) {
				case 0:
					var _n1 = _n0.a;
					return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
				case 3:
					var urlRequest = _n0.a.a;
					if (!urlRequest.$) {
						var url = urlRequest.a;
						var _n3 = url.b8;
						if (_n3.$ === 1) {
							return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
						} else {
							return _Utils_Tuple2(
								model,
								A2(
									elm$browser$Browser$Navigation$pushUrl,
									author$project$Session$navKey(
										author$project$Main$toSession(model)),
									elm$url$Url$toString(url)));
						}
					} else {
						var href = urlRequest.a;
						return _Utils_Tuple2(
							model,
							elm$browser$Browser$Navigation$load(href));
					}
				case 2:
					var url = _n0.a.a;
					return A2(
						author$project$Main$changeRouteTo,
						author$project$Route$fromUrl(url),
						model);
				case 1:
					var route = _n0.a.a;
					return A2(author$project$Main$changeRouteTo, route, model);
				case 4:
					if (_n0.b.$ === 2) {
						var subMsg = _n0.a.a;
						var dashboard = _n0.b.a;
						return A4(
							author$project$Main$updateWith,
							author$project$Main$Dashboard,
							author$project$Main$GotDashboardMsg,
							model,
							A2(author$project$Page$Dashboard$update, subMsg, dashboard));
					} else {
						break _n0$12;
					}
				case 9:
					if (_n0.b.$ === 7) {
						var subMsg = _n0.a.a;
						var login = _n0.b.a;
						return A4(
							author$project$Main$updateWith,
							author$project$Main$Login,
							author$project$Main$GotLoginMsg,
							model,
							A2(author$project$Page$Login$update, subMsg, login));
					} else {
						break _n0$12;
					}
				case 5:
					if (_n0.b.$ === 3) {
						var subMsg = _n0.a.a;
						var upload = _n0.b.a;
						return A4(
							author$project$Main$updateWith,
							author$project$Main$Upload,
							author$project$Main$GotUploadMsg,
							model,
							A2(author$project$Page$Upload$update, subMsg, upload));
					} else {
						break _n0$12;
					}
				case 6:
					if (_n0.b.$ === 4) {
						var subMsg = _n0.a.a;
						var portfolio = _n0.b.a;
						return A4(
							author$project$Main$updateWith,
							author$project$Main$Portfolio,
							author$project$Main$GotPortfolioMsg,
							model,
							A2(author$project$Page$Portfolio$update, subMsg, portfolio));
					} else {
						break _n0$12;
					}
				case 7:
					if (_n0.b.$ === 5) {
						var subMsg = _n0.a.a;
						var charts = _n0.b.a;
						return A4(
							author$project$Main$updateWith,
							author$project$Main$Charts,
							author$project$Main$GotChartsMsg,
							model,
							A2(author$project$Page$Charts$update, subMsg, charts));
					} else {
						break _n0$12;
					}
				case 8:
					if (_n0.b.$ === 6) {
						var subMsg = _n0.a.a;
						var accbalance = _n0.b.a;
						return A4(
							author$project$Main$updateWith,
							author$project$Main$AccountBalance,
							author$project$Main$GotAccountBalanceMsg,
							model,
							A2(author$project$Page$AccountBalance$update, subMsg, accbalance));
					} else {
						break _n0$12;
					}
				case 10:
					if (_n0.b.$ === 8) {
						var subMsg = _n0.a.a;
						var apidoc = _n0.b.a;
						return A4(
							author$project$Main$updateWith,
							author$project$Main$ApiDocument,
							author$project$Main$GotApiDocumentMsg,
							model,
							A2(author$project$Page$ApiDocument$update, subMsg, apidoc));
					} else {
						break _n0$12;
					}
				default:
					if (!_n0.b.$) {
						var session = _n0.a.a;
						return _Utils_Tuple2(
							author$project$Main$Redirect(session),
							A2(
								author$project$Route$replaceUrl,
								author$project$Session$navKey(session),
								author$project$Route$Dashboard));
					} else {
						break _n0$12;
					}
			}
		}
		return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
	});
var author$project$Main$Ignored = {$: 0};
var elm$html$Html$a = _VirtualDom_node('a');
var elm$html$Html$img = _VirtualDom_node('img');
var elm$html$Html$span = _VirtualDom_node('span');
var elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var elm$html$Html$text = elm$virtual_dom$VirtualDom$text;
var elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			elm$json$Json$Encode$string(string));
	});
var elm$html$Html$Attributes$alt = elm$html$Html$Attributes$stringProperty('alt');
var elm$html$Html$Attributes$height = function (n) {
	return A2(
		_VirtualDom_attribute,
		'height',
		elm$core$String$fromInt(n));
};
var elm$html$Html$Attributes$href = function (url) {
	return A2(
		elm$html$Html$Attributes$stringProperty,
		'href',
		_VirtualDom_noJavaScriptUri(url));
};
var elm$html$Html$Attributes$src = function (url) {
	return A2(
		elm$html$Html$Attributes$stringProperty,
		'src',
		_VirtualDom_noJavaScriptOrHtmlUri(url));
};
var elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var elm$html$Html$Attributes$style = elm$virtual_dom$VirtualDom$style;
var elm$html$Html$Attributes$width = function (n) {
	return A2(
		_VirtualDom_attribute,
		'width',
		elm$core$String$fromInt(n));
};
var elm$html$Html$Attributes$class = elm$html$Html$Attributes$stringProperty('className');
var surprisetalk$elm_bulma$Bulma$Classes$content = elm$html$Html$Attributes$class('content');
var surprisetalk$elm_bulma$Bulma$Classes$isLarge = elm$html$Html$Attributes$class('is-large');
var surprisetalk$elm_bulma$Bulma$Classes$isMedium = elm$html$Html$Attributes$class('is-medium');
var surprisetalk$elm_bulma$Bulma$Classes$isSmall = elm$html$Html$Attributes$class('is-small');
var surprisetalk$elm_bulma$Bulma$Classes$none = elm$html$Html$Attributes$class('');
var elm$virtual_dom$VirtualDom$node = function (tag) {
	return _VirtualDom_node(
		_VirtualDom_noScript(tag));
};
var elm$html$Html$node = elm$virtual_dom$VirtualDom$node;
var surprisetalk$elm_bulma$Helpers$node = F3(
	function (tag, attrs_, attrs) {
		return A2(
			elm$html$Html$node,
			tag,
			_Utils_ap(attrs, attrs_));
	});
var surprisetalk$elm_bulma$Bulma$Elements$content = function (size) {
	return A2(
		surprisetalk$elm_bulma$Helpers$node,
		'div',
		_List_fromArray(
			[
				surprisetalk$elm_bulma$Bulma$Classes$content,
				function () {
				switch (size) {
					case 0:
						return surprisetalk$elm_bulma$Bulma$Classes$isSmall;
					case 1:
						return surprisetalk$elm_bulma$Bulma$Classes$none;
					case 2:
						return surprisetalk$elm_bulma$Bulma$Classes$isMedium;
					default:
						return surprisetalk$elm_bulma$Bulma$Classes$isLarge;
				}
			}()
			]));
};
var surprisetalk$elm_bulma$Bulma$Classes$container = elm$html$Html$Attributes$class('container');
var surprisetalk$elm_bulma$Bulma$Layout$container = A2(
	surprisetalk$elm_bulma$Helpers$node,
	'div',
	_List_fromArray(
		[surprisetalk$elm_bulma$Bulma$Classes$container]));
var surprisetalk$elm_bulma$Bulma$Classes$footer = elm$html$Html$Attributes$class('footer');
var surprisetalk$elm_bulma$Bulma$Layout$footer = A2(
	surprisetalk$elm_bulma$Helpers$node,
	'footer',
	_List_fromArray(
		[surprisetalk$elm_bulma$Bulma$Classes$footer]));
var surprisetalk$elm_bulma$Bulma$Classes$is1 = elm$html$Html$Attributes$class('is-1');
var surprisetalk$elm_bulma$Bulma$Classes$is10 = elm$html$Html$Attributes$class('is-10');
var surprisetalk$elm_bulma$Bulma$Classes$is11 = elm$html$Html$Attributes$class('is-11');
var surprisetalk$elm_bulma$Bulma$Classes$is2 = elm$html$Html$Attributes$class('is-2');
var surprisetalk$elm_bulma$Bulma$Classes$is3 = elm$html$Html$Attributes$class('is-3');
var surprisetalk$elm_bulma$Bulma$Classes$is4 = elm$html$Html$Attributes$class('is-4');
var surprisetalk$elm_bulma$Bulma$Classes$is5 = elm$html$Html$Attributes$class('is-5');
var surprisetalk$elm_bulma$Bulma$Classes$is6 = elm$html$Html$Attributes$class('is-6');
var surprisetalk$elm_bulma$Bulma$Classes$is7 = elm$html$Html$Attributes$class('is-7');
var surprisetalk$elm_bulma$Bulma$Classes$is8 = elm$html$Html$Attributes$class('is-8');
var surprisetalk$elm_bulma$Bulma$Classes$is9 = elm$html$Html$Attributes$class('is-9');
var surprisetalk$elm_bulma$Bulma$Classes$isAncestor = elm$html$Html$Attributes$class('is-ancestor');
var surprisetalk$elm_bulma$Bulma$Classes$tile = elm$html$Html$Attributes$class('tile');
var surprisetalk$elm_bulma$Bulma$Layout$tileAncestor = function (width) {
	return A2(
		surprisetalk$elm_bulma$Helpers$node,
		'div',
		_List_fromArray(
			[
				surprisetalk$elm_bulma$Bulma$Classes$tile,
				surprisetalk$elm_bulma$Bulma$Classes$isAncestor,
				function () {
				switch (width) {
					case 0:
						return surprisetalk$elm_bulma$Bulma$Classes$none;
					case 1:
						return surprisetalk$elm_bulma$Bulma$Classes$is1;
					case 2:
						return surprisetalk$elm_bulma$Bulma$Classes$is2;
					case 3:
						return surprisetalk$elm_bulma$Bulma$Classes$is3;
					case 4:
						return surprisetalk$elm_bulma$Bulma$Classes$is4;
					case 5:
						return surprisetalk$elm_bulma$Bulma$Classes$is5;
					case 6:
						return surprisetalk$elm_bulma$Bulma$Classes$is6;
					case 7:
						return surprisetalk$elm_bulma$Bulma$Classes$is7;
					case 8:
						return surprisetalk$elm_bulma$Bulma$Classes$is8;
					case 9:
						return surprisetalk$elm_bulma$Bulma$Classes$is9;
					case 10:
						return surprisetalk$elm_bulma$Bulma$Classes$is10;
					default:
						return surprisetalk$elm_bulma$Bulma$Classes$is11;
				}
			}()
			]));
};
var surprisetalk$elm_bulma$Bulma$Classes$isChild = elm$html$Html$Attributes$class('is-child');
var surprisetalk$elm_bulma$Bulma$Layout$tileChild = function (width) {
	return A2(
		surprisetalk$elm_bulma$Helpers$node,
		'div',
		_List_fromArray(
			[
				surprisetalk$elm_bulma$Bulma$Classes$tile,
				surprisetalk$elm_bulma$Bulma$Classes$isChild,
				function () {
				switch (width) {
					case 0:
						return surprisetalk$elm_bulma$Bulma$Classes$none;
					case 1:
						return surprisetalk$elm_bulma$Bulma$Classes$is1;
					case 2:
						return surprisetalk$elm_bulma$Bulma$Classes$is2;
					case 3:
						return surprisetalk$elm_bulma$Bulma$Classes$is3;
					case 4:
						return surprisetalk$elm_bulma$Bulma$Classes$is4;
					case 5:
						return surprisetalk$elm_bulma$Bulma$Classes$is5;
					case 6:
						return surprisetalk$elm_bulma$Bulma$Classes$is6;
					case 7:
						return surprisetalk$elm_bulma$Bulma$Classes$is7;
					case 8:
						return surprisetalk$elm_bulma$Bulma$Classes$is8;
					case 9:
						return surprisetalk$elm_bulma$Bulma$Classes$is9;
					case 10:
						return surprisetalk$elm_bulma$Bulma$Classes$is10;
					default:
						return surprisetalk$elm_bulma$Bulma$Classes$is11;
				}
			}()
			]));
};
var surprisetalk$elm_bulma$Bulma$Classes$isVertical = elm$html$Html$Attributes$class('is-vertical');
var surprisetalk$elm_bulma$Bulma$Layout$verticalTile = function (width) {
	return A2(
		surprisetalk$elm_bulma$Helpers$node,
		'div',
		_List_fromArray(
			[
				surprisetalk$elm_bulma$Bulma$Classes$tile,
				surprisetalk$elm_bulma$Bulma$Classes$isVertical,
				function () {
				switch (width) {
					case 0:
						return surprisetalk$elm_bulma$Bulma$Classes$none;
					case 1:
						return surprisetalk$elm_bulma$Bulma$Classes$is1;
					case 2:
						return surprisetalk$elm_bulma$Bulma$Classes$is2;
					case 3:
						return surprisetalk$elm_bulma$Bulma$Classes$is3;
					case 4:
						return surprisetalk$elm_bulma$Bulma$Classes$is4;
					case 5:
						return surprisetalk$elm_bulma$Bulma$Classes$is5;
					case 6:
						return surprisetalk$elm_bulma$Bulma$Classes$is6;
					case 7:
						return surprisetalk$elm_bulma$Bulma$Classes$is7;
					case 8:
						return surprisetalk$elm_bulma$Bulma$Classes$is8;
					case 9:
						return surprisetalk$elm_bulma$Bulma$Classes$is9;
					case 10:
						return surprisetalk$elm_bulma$Bulma$Classes$is10;
					default:
						return surprisetalk$elm_bulma$Bulma$Classes$is11;
				}
			}()
			]));
};
var surprisetalk$elm_bulma$Bulma$Modifiers$Auto = 0;
var surprisetalk$elm_bulma$Bulma$Modifiers$Standard = 1;
var surprisetalk$elm_bulma$Bulma$Modifiers$Width2 = 2;
var surprisetalk$elm_bulma$Bulma$Modifiers$Width5 = 5;
var surprisetalk$elm_bulma$Bulma$Modifiers$Typography$BlackLight = 1;
var surprisetalk$elm_bulma$Bulma$Modifiers$Typography$Medium = 4;
var surprisetalk$elm_bulma$Bulma$Modifiers$Typography$Standard = 5;
var surprisetalk$elm_bulma$Bulma$Modifiers$Typography$textColor = function (color) {
	return elm$html$Html$Attributes$class(
		function () {
			switch (color) {
				case 0:
					return 'has-text-black';
				case 1:
					return 'has-text-black-bis';
				case 2:
					return 'has-text-black-ter';
				case 3:
					return 'has-text-grey-darker';
				case 4:
					return 'has-text-grey-dark';
				case 5:
					return 'has-text-grey';
				case 6:
					return 'has-text-grey-light';
				case 7:
					return 'has-text-grey-lighter';
				case 8:
					return 'has-text-white-bis';
				case 9:
					return 'has-text-white-ter';
				case 10:
					return 'has-text-white';
				case 11:
					return 'has-text-primary';
				case 12:
					return 'has-text-info';
				case 13:
					return 'has-text-success';
				case 14:
					return 'has-text-warning';
				default:
					return 'has-text-danger';
			}
		}());
};
var surprisetalk$elm_bulma$Bulma$Classes$isSize1 = elm$html$Html$Attributes$class('is-size-1');
var surprisetalk$elm_bulma$Bulma$Classes$isSize2 = elm$html$Html$Attributes$class('is-size-2');
var surprisetalk$elm_bulma$Bulma$Classes$isSize3 = elm$html$Html$Attributes$class('is-size-3');
var surprisetalk$elm_bulma$Bulma$Classes$isSize4 = elm$html$Html$Attributes$class('is-size-4');
var surprisetalk$elm_bulma$Bulma$Classes$isSize5 = elm$html$Html$Attributes$class('is-size-5');
var surprisetalk$elm_bulma$Bulma$Classes$isSize6 = elm$html$Html$Attributes$class('is-size-6');
var surprisetalk$elm_bulma$Bulma$Classes$isSize7 = elm$html$Html$Attributes$class('is-size-7');
var surprisetalk$elm_bulma$Bulma$Modifiers$Typography$textSize = function (ts) {
	switch (ts) {
		case 0:
			return surprisetalk$elm_bulma$Bulma$Classes$isSize1;
		case 1:
			return surprisetalk$elm_bulma$Bulma$Classes$isSize2;
		case 2:
			return surprisetalk$elm_bulma$Bulma$Classes$isSize3;
		case 3:
			return surprisetalk$elm_bulma$Bulma$Classes$isSize4;
		case 4:
			return surprisetalk$elm_bulma$Bulma$Classes$isSize5;
		case 5:
			return surprisetalk$elm_bulma$Bulma$Classes$isSize6;
		default:
			return surprisetalk$elm_bulma$Bulma$Classes$isSize7;
	}
};
var author$project$Page$viewFooter = function () {
	var margin = A2(elm$html$Html$Attributes$style, 'margin', '1rem');
	var viewOnGitHub = A3(
		surprisetalk$elm_bulma$Bulma$Elements$content,
		1,
		_List_fromArray(
			[margin]),
		_List_fromArray(
			[
				elm$html$Html$text('view on '),
				A2(
				elm$html$Html$a,
				_List_fromArray(
					[
						elm$html$Html$Attributes$href('https://github.com/ak1211/tractor')
					]),
				_List_fromArray(
					[
						A2(
						elm$html$Html$img,
						_List_fromArray(
							[
								A2(elm$html$Html$Attributes$style, 'margin', '0 10px'),
								A2(elm$html$Html$Attributes$style, 'vertical-align', 'middle'),
								elm$html$Html$Attributes$src('public/assets/GitHub-Mark-32px.png'),
								elm$html$Html$Attributes$alt('GitHub Logo'),
								elm$html$Html$Attributes$width(24),
								elm$html$Html$Attributes$height(24)
							]),
						_List_Nil)
					]))
			]));
	var copy = A3(
		surprisetalk$elm_bulma$Bulma$Elements$content,
		1,
		_List_fromArray(
			[
				surprisetalk$elm_bulma$Bulma$Modifiers$Typography$textSize(5),
				margin
			]),
		_List_fromArray(
			[
				elm$html$Html$text('The Assets observation application '),
				A2(
				elm$html$Html$span,
				_List_fromArray(
					[
						surprisetalk$elm_bulma$Bulma$Modifiers$Typography$textSize(4),
						surprisetalk$elm_bulma$Bulma$Modifiers$Typography$textColor(1),
						A2(elm$html$Html$Attributes$style, 'font-family', '\'Gugi\', cursive')
					]),
				_List_fromArray(
					[
						elm$html$Html$text('TRACTOR')
					])),
				elm$html$Html$text('.')
			]));
	var bulma = A3(
		surprisetalk$elm_bulma$Bulma$Elements$content,
		1,
		_List_fromArray(
			[margin]),
		_List_fromArray(
			[
				A2(
				elm$html$Html$a,
				_List_fromArray(
					[
						elm$html$Html$Attributes$href('https://bulma.io')
					]),
				_List_fromArray(
					[
						A2(
						elm$html$Html$img,
						_List_fromArray(
							[
								elm$html$Html$Attributes$src('public/assets/made-with-bulma.png'),
								elm$html$Html$Attributes$alt('Made with Bulma'),
								elm$html$Html$Attributes$width(128),
								elm$html$Html$Attributes$height(24)
							]),
						_List_Nil)
					]))
			]));
	return A2(
		surprisetalk$elm_bulma$Bulma$Layout$footer,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				surprisetalk$elm_bulma$Bulma$Layout$container,
				_List_Nil,
				_List_fromArray(
					[
						A3(
						surprisetalk$elm_bulma$Bulma$Layout$tileAncestor,
						0,
						_List_Nil,
						_List_fromArray(
							[
								A3(
								surprisetalk$elm_bulma$Bulma$Layout$verticalTile,
								5,
								_List_Nil,
								_List_fromArray(
									[
										A3(
										surprisetalk$elm_bulma$Bulma$Layout$tileChild,
										0,
										_List_Nil,
										_List_fromArray(
											[copy]))
									])),
								A3(
								surprisetalk$elm_bulma$Bulma$Layout$verticalTile,
								2,
								_List_Nil,
								_List_fromArray(
									[
										A3(
										surprisetalk$elm_bulma$Bulma$Layout$tileChild,
										0,
										_List_Nil,
										_List_fromArray(
											[viewOnGitHub]))
									])),
								A3(
								surprisetalk$elm_bulma$Bulma$Layout$verticalTile,
								2,
								_List_Nil,
								_List_fromArray(
									[
										A3(
										surprisetalk$elm_bulma$Bulma$Layout$tileChild,
										0,
										_List_Nil,
										_List_fromArray(
											[bulma]))
									])),
								A3(surprisetalk$elm_bulma$Bulma$Layout$verticalTile, 0, _List_Nil, _List_Nil)
							]))
					]))
			]));
}();
var surprisetalk$elm_bulma$Bulma$Elements$H1 = 0;
var surprisetalk$elm_bulma$Bulma$Classes$title = elm$html$Html$Attributes$class('title');
var surprisetalk$elm_bulma$Bulma$Elements$title = function (size) {
	return A2(
		surprisetalk$elm_bulma$Helpers$node,
		function () {
			switch (size) {
				case 0:
					return 'h1';
				case 1:
					return 'h2';
				case 2:
					return 'h3';
				case 3:
					return 'h4';
				case 4:
					return 'h5';
				default:
					return 'h6';
			}
		}(),
		_List_fromArray(
			[
				surprisetalk$elm_bulma$Bulma$Classes$title,
				function () {
				switch (size) {
					case 0:
						return surprisetalk$elm_bulma$Bulma$Classes$is1;
					case 1:
						return surprisetalk$elm_bulma$Bulma$Classes$is2;
					case 2:
						return surprisetalk$elm_bulma$Bulma$Classes$is3;
					case 3:
						return surprisetalk$elm_bulma$Bulma$Classes$is4;
					case 4:
						return surprisetalk$elm_bulma$Bulma$Classes$is5;
					default:
						return surprisetalk$elm_bulma$Bulma$Classes$is6;
				}
			}()
			]));
};
var surprisetalk$elm_bulma$Bulma$Classes$hero = elm$html$Html$Attributes$class('hero');
var surprisetalk$elm_bulma$Bulma$Classes$isBlack = elm$html$Html$Attributes$class('is-black');
var surprisetalk$elm_bulma$Bulma$Classes$isBold = elm$html$Html$Attributes$class('is-bold');
var surprisetalk$elm_bulma$Bulma$Classes$isDanger = elm$html$Html$Attributes$class('is-danger');
var surprisetalk$elm_bulma$Bulma$Classes$isDark = elm$html$Html$Attributes$class('is-dark');
var surprisetalk$elm_bulma$Bulma$Classes$isFullHeight = elm$html$Html$Attributes$class('is-fullheight');
var surprisetalk$elm_bulma$Bulma$Classes$isInfo = elm$html$Html$Attributes$class('is-info');
var surprisetalk$elm_bulma$Bulma$Classes$isLight = elm$html$Html$Attributes$class('is-light');
var surprisetalk$elm_bulma$Bulma$Classes$isLink = elm$html$Html$Attributes$class('is-link');
var surprisetalk$elm_bulma$Bulma$Classes$isPrimary = elm$html$Html$Attributes$class('is-primary');
var surprisetalk$elm_bulma$Bulma$Classes$isSuccess = elm$html$Html$Attributes$class('is-success');
var surprisetalk$elm_bulma$Bulma$Classes$isWarning = elm$html$Html$Attributes$class('is-warning');
var surprisetalk$elm_bulma$Bulma$Classes$isWhite = elm$html$Html$Attributes$class('is-white');
var surprisetalk$elm_bulma$Bulma$Layout$hero = function (_n0) {
	var bold = _n0.aj;
	var size = _n0.q;
	var color = _n0.b_;
	return A2(
		surprisetalk$elm_bulma$Helpers$node,
		'section',
		_List_fromArray(
			[
				surprisetalk$elm_bulma$Bulma$Classes$hero,
				function () {
				if (bold) {
					return surprisetalk$elm_bulma$Bulma$Classes$isBold;
				} else {
					return surprisetalk$elm_bulma$Bulma$Classes$none;
				}
			}(),
				function () {
				switch (size) {
					case 0:
						return surprisetalk$elm_bulma$Bulma$Classes$none;
					case 1:
						return surprisetalk$elm_bulma$Bulma$Classes$isMedium;
					case 2:
						return surprisetalk$elm_bulma$Bulma$Classes$isLarge;
					default:
						return surprisetalk$elm_bulma$Bulma$Classes$isFullHeight;
				}
			}(),
				function () {
				switch (color) {
					case 0:
						return surprisetalk$elm_bulma$Bulma$Classes$none;
					case 1:
						return surprisetalk$elm_bulma$Bulma$Classes$isWhite;
					case 4:
						return surprisetalk$elm_bulma$Bulma$Classes$isBlack;
					case 2:
						return surprisetalk$elm_bulma$Bulma$Classes$isLight;
					case 3:
						return surprisetalk$elm_bulma$Bulma$Classes$isDark;
					case 5:
						return surprisetalk$elm_bulma$Bulma$Classes$isPrimary;
					case 7:
						return surprisetalk$elm_bulma$Bulma$Classes$isInfo;
					case 8:
						return surprisetalk$elm_bulma$Bulma$Classes$isSuccess;
					case 9:
						return surprisetalk$elm_bulma$Bulma$Classes$isWarning;
					case 10:
						return surprisetalk$elm_bulma$Bulma$Classes$isDanger;
					default:
						return surprisetalk$elm_bulma$Bulma$Classes$isLink;
				}
			}()
			]));
};
var surprisetalk$elm_bulma$Bulma$Classes$heroBody = elm$html$Html$Attributes$class('hero-body');
var surprisetalk$elm_bulma$Bulma$Layout$heroBody = A2(
	surprisetalk$elm_bulma$Helpers$node,
	'div',
	_List_fromArray(
		[surprisetalk$elm_bulma$Bulma$Classes$heroBody]));
var surprisetalk$elm_bulma$Bulma$Modifiers$Default = 0;
var surprisetalk$elm_bulma$Bulma$Modifiers$Small = 0;
var surprisetalk$elm_bulma$Bulma$Layout$heroModifiers = {aj: false, b_: 0, q: 0};
var author$project$Page$viewHero = function (title) {
	return A3(
		surprisetalk$elm_bulma$Bulma$Layout$hero,
		surprisetalk$elm_bulma$Bulma$Layout$heroModifiers,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				surprisetalk$elm_bulma$Bulma$Layout$heroBody,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						surprisetalk$elm_bulma$Bulma$Layout$container,
						_List_Nil,
						_List_fromArray(
							[
								A3(
								surprisetalk$elm_bulma$Bulma$Elements$title,
								0,
								_List_fromArray(
									[
										A2(elm$html$Html$Attributes$style, 'border-left', 'solid 7px burlywood'),
										A2(elm$html$Html$Attributes$style, 'padding', '0.8rem 1.2rem')
									]),
								_List_fromArray(
									[
										elm$html$Html$text(title)
									]))
							]))
					]))
			]));
};
var author$project$Page$alwaysStop = function (msg) {
	return _Utils_Tuple2(msg, true);
};
var elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 1, a: a};
};
var elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var elm$html$Html$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			elm$virtual_dom$VirtualDom$on,
			event,
			elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var author$project$Page$stopPropagationOnClick = function (message) {
	return A2(
		elm$html$Html$Events$stopPropagationOn,
		'click',
		A2(
			elm$json$Json$Decode$map,
			author$project$Page$alwaysStop,
			elm$json$Json$Decode$succeed(message)));
};
var author$project$Route$href = function (targetRoute) {
	return elm$html$Html$Attributes$href(
		author$project$Route$routeToString(targetRoute));
};
var elm$html$Html$div = _VirtualDom_node('div');
var elm$html$Html$i = _VirtualDom_node('i');
var surprisetalk$elm_bulma$Bulma$Classes$isTransparent = elm$html$Html$Attributes$class('is-transparent');
var surprisetalk$elm_bulma$Bulma$Classes$navbar = elm$html$Html$Attributes$class('navbar');
var surprisetalk$elm_bulma$Bulma$Components$navbar = function (_n0) {
	var color = _n0.b_;
	var transparent = _n0.aM;
	return A2(
		surprisetalk$elm_bulma$Helpers$node,
		'nav',
		_List_fromArray(
			[
				surprisetalk$elm_bulma$Bulma$Classes$navbar,
				function () {
				if (transparent) {
					return surprisetalk$elm_bulma$Bulma$Classes$isTransparent;
				} else {
					return surprisetalk$elm_bulma$Bulma$Classes$none;
				}
			}(),
				function () {
				switch (color) {
					case 0:
						return surprisetalk$elm_bulma$Bulma$Classes$none;
					case 1:
						return surprisetalk$elm_bulma$Bulma$Classes$isWhite;
					case 2:
						return surprisetalk$elm_bulma$Bulma$Classes$isLight;
					case 3:
						return surprisetalk$elm_bulma$Bulma$Classes$isDark;
					case 4:
						return surprisetalk$elm_bulma$Bulma$Classes$isBlack;
					case 5:
						return surprisetalk$elm_bulma$Bulma$Classes$isPrimary;
					case 6:
						return surprisetalk$elm_bulma$Bulma$Classes$isLink;
					case 7:
						return surprisetalk$elm_bulma$Bulma$Classes$isInfo;
					case 8:
						return surprisetalk$elm_bulma$Bulma$Classes$isSuccess;
					case 9:
						return surprisetalk$elm_bulma$Bulma$Classes$isWarning;
					default:
						return surprisetalk$elm_bulma$Bulma$Classes$isDanger;
				}
			}()
			]));
};
var surprisetalk$elm_bulma$Bulma$Components$fixedNavbar = F2(
	function (dir, mods) {
		return A2(
			elm$core$Basics$composeL,
			surprisetalk$elm_bulma$Bulma$Components$navbar(mods),
			elm$core$List$cons(
				function () {
					if (!dir) {
						return elm$html$Html$Attributes$class('is-fixed-top');
					} else {
						return elm$html$Html$Attributes$class('is-fixed-bottom');
					}
				}()));
	});
var surprisetalk$elm_bulma$Bulma$Components$navbarBrand = F3(
	function (attrs, burger, items) {
		return A2(
			elm$html$Html$div,
			A2(
				elm$core$List$cons,
				elm$html$Html$Attributes$class('navbar-brand'),
				attrs),
			_Utils_ap(
				items,
				_List_fromArray(
					[burger])));
	});
var surprisetalk$elm_bulma$Bulma$Classes$isActive = elm$html$Html$Attributes$class('is-active');
var surprisetalk$elm_bulma$Bulma$Classes$navbarBurger = elm$html$Html$Attributes$class('navbar-burger');
var surprisetalk$elm_bulma$Bulma$Components$navbarBurger = function (isActive) {
	return A2(
		surprisetalk$elm_bulma$Helpers$node,
		'a',
		_List_fromArray(
			[
				surprisetalk$elm_bulma$Bulma$Classes$navbarBurger,
				isActive ? surprisetalk$elm_bulma$Bulma$Classes$isActive : surprisetalk$elm_bulma$Bulma$Classes$none
			]));
};
var surprisetalk$elm_bulma$Bulma$Classes$navbarEnd = elm$html$Html$Attributes$class('navbar-end');
var surprisetalk$elm_bulma$Bulma$Components$navbarEnd = A2(
	surprisetalk$elm_bulma$Helpers$node,
	'div',
	_List_fromArray(
		[surprisetalk$elm_bulma$Bulma$Classes$navbarEnd]));
var surprisetalk$elm_bulma$Bulma$Classes$navbarItem = elm$html$Html$Attributes$class('navbar-item');
var surprisetalk$elm_bulma$Bulma$Components$navbarItem = function (isActive) {
	return A2(
		surprisetalk$elm_bulma$Helpers$node,
		'div',
		_List_fromArray(
			[
				surprisetalk$elm_bulma$Bulma$Classes$navbarItem,
				isActive ? surprisetalk$elm_bulma$Bulma$Classes$isActive : surprisetalk$elm_bulma$Bulma$Classes$none
			]));
};
var surprisetalk$elm_bulma$Bulma$Components$navbarItemLink = function (isActive) {
	return A2(
		surprisetalk$elm_bulma$Helpers$node,
		'a',
		_List_fromArray(
			[
				surprisetalk$elm_bulma$Bulma$Classes$navbarItem,
				isActive ? surprisetalk$elm_bulma$Bulma$Classes$isActive : surprisetalk$elm_bulma$Bulma$Classes$none
			]));
};
var surprisetalk$elm_bulma$Bulma$Classes$navbarMenu = elm$html$Html$Attributes$class('navbar-menu');
var surprisetalk$elm_bulma$Bulma$Components$navbarMenu = F2(
	function (isActive, attrs) {
		return A3(
			surprisetalk$elm_bulma$Helpers$node,
			'div',
			_List_fromArray(
				[
					surprisetalk$elm_bulma$Bulma$Classes$navbarMenu,
					isActive ? surprisetalk$elm_bulma$Bulma$Classes$isActive : surprisetalk$elm_bulma$Bulma$Classes$none
				]),
			attrs);
	});
var surprisetalk$elm_bulma$Bulma$Components$navbarModifiers = {b_: 0, aM: false};
var surprisetalk$elm_bulma$Bulma$Classes$navbarStart = elm$html$Html$Attributes$class('navbar-start');
var surprisetalk$elm_bulma$Bulma$Components$navbarStart = A2(
	surprisetalk$elm_bulma$Helpers$node,
	'div',
	_List_fromArray(
		[surprisetalk$elm_bulma$Bulma$Classes$navbarStart]));
var elm$json$Json$Encode$bool = _Json_wrap;
var elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			elm$json$Json$Encode$bool(bool));
	});
var elm$html$Html$Attributes$disabled = elm$html$Html$Attributes$boolProperty('disabled');
var surprisetalk$elm_bulma$Bulma$Classes$button = elm$html$Html$Attributes$class('button');
var surprisetalk$elm_bulma$Bulma$Classes$isFocused = elm$html$Html$Attributes$class('is-focused');
var surprisetalk$elm_bulma$Bulma$Classes$isHovered = elm$html$Html$Attributes$class('is-hovered');
var surprisetalk$elm_bulma$Bulma$Classes$isInverted = elm$html$Html$Attributes$class('is-inverted');
var surprisetalk$elm_bulma$Bulma$Classes$isLoading = elm$html$Html$Attributes$class('is-loading');
var surprisetalk$elm_bulma$Bulma$Classes$isOutlined = elm$html$Html$Attributes$class('is-outlined');
var surprisetalk$elm_bulma$Bulma$Classes$isRounded = elm$html$Html$Attributes$class('is-rounded');
var surprisetalk$elm_bulma$Bulma$Classes$isSelected = elm$html$Html$Attributes$class('is-selected');
var surprisetalk$elm_bulma$Bulma$Classes$isStatic = elm$html$Html$Attributes$class('is-static');
var surprisetalk$elm_bulma$Bulma$Classes$icon = elm$html$Html$Attributes$class('icon');
var surprisetalk$elm_bulma$Bulma$Elements$icon = function (size) {
	return A2(
		surprisetalk$elm_bulma$Helpers$node,
		'span',
		_List_fromArray(
			[
				surprisetalk$elm_bulma$Bulma$Classes$icon,
				function () {
				switch (size) {
					case 0:
						return surprisetalk$elm_bulma$Bulma$Classes$isSmall;
					case 1:
						return surprisetalk$elm_bulma$Bulma$Classes$none;
					case 2:
						return surprisetalk$elm_bulma$Bulma$Classes$isMedium;
					default:
						return surprisetalk$elm_bulma$Bulma$Classes$isLarge;
				}
			}()
			]));
};
var surprisetalk$elm_bulma$Bulma$Elements$button = F3(
	function (_n0, attrs, htmls) {
		var disabled = _n0.ao;
		var outlined = _n0.aD;
		var inverted = _n0.aw;
		var rounded = _n0.aF;
		var size = _n0.q;
		var state = _n0.bC;
		var color = _n0.b_;
		var _static = _n0.aI;
		var iconLeft = _n0.au;
		var iconRight = _n0.av;
		var iconRight_ = function () {
			if (!iconRight.$) {
				var _n13 = iconRight.a;
				var size_ = _n13.a;
				var attrs_ = _n13.b;
				var body = _n13.c;
				return _List_fromArray(
					[
						A3(
						surprisetalk$elm_bulma$Bulma$Elements$icon,
						size_,
						attrs_,
						_List_fromArray(
							[body]))
					]);
			} else {
				return _List_Nil;
			}
		}();
		var iconLeft_ = function () {
			if (!iconLeft.$) {
				var _n11 = iconLeft.a;
				var size_ = _n11.a;
				var attrs_ = _n11.b;
				var body = _n11.c;
				return _List_fromArray(
					[
						A3(
						surprisetalk$elm_bulma$Bulma$Elements$icon,
						size_,
						attrs_,
						_List_fromArray(
							[body]))
					]);
			} else {
				return _List_Nil;
			}
		}();
		var htmls_ = function () {
			if (!htmls.b) {
				return _Utils_ap(iconLeft_, iconRight_);
			} else {
				var htmls__ = htmls;
				return _Utils_ap(
					iconLeft_,
					_Utils_ap(
						_List_fromArray(
							[
								A2(elm$html$Html$span, _List_Nil, htmls__)
							]),
						iconRight_));
			}
		}();
		return A4(
			surprisetalk$elm_bulma$Helpers$node,
			'button',
			_List_fromArray(
				[
					elm$html$Html$Attributes$disabled(disabled),
					surprisetalk$elm_bulma$Bulma$Classes$button,
					function () {
					if (_static) {
						return surprisetalk$elm_bulma$Bulma$Classes$isStatic;
					} else {
						return surprisetalk$elm_bulma$Bulma$Classes$none;
					}
				}(),
					function () {
					if (outlined) {
						return surprisetalk$elm_bulma$Bulma$Classes$isOutlined;
					} else {
						return surprisetalk$elm_bulma$Bulma$Classes$none;
					}
				}(),
					function () {
					if (inverted) {
						return surprisetalk$elm_bulma$Bulma$Classes$isInverted;
					} else {
						return surprisetalk$elm_bulma$Bulma$Classes$none;
					}
				}(),
					function () {
					if (rounded) {
						return surprisetalk$elm_bulma$Bulma$Classes$isRounded;
					} else {
						return surprisetalk$elm_bulma$Bulma$Classes$none;
					}
				}(),
					function () {
					if (!color) {
						return surprisetalk$elm_bulma$Bulma$Classes$none;
					} else {
						return surprisetalk$elm_bulma$Bulma$Classes$isSelected;
					}
				}(),
					function () {
					switch (color) {
						case 0:
							return surprisetalk$elm_bulma$Bulma$Classes$none;
						case 1:
							return surprisetalk$elm_bulma$Bulma$Classes$isWhite;
						case 2:
							return surprisetalk$elm_bulma$Bulma$Classes$isLight;
						case 3:
							return surprisetalk$elm_bulma$Bulma$Classes$isDark;
						case 4:
							return surprisetalk$elm_bulma$Bulma$Classes$isBlack;
						case 5:
							return surprisetalk$elm_bulma$Bulma$Classes$isPrimary;
						case 6:
							return surprisetalk$elm_bulma$Bulma$Classes$isLink;
						case 7:
							return surprisetalk$elm_bulma$Bulma$Classes$isInfo;
						case 8:
							return surprisetalk$elm_bulma$Bulma$Classes$isSuccess;
						case 9:
							return surprisetalk$elm_bulma$Bulma$Classes$isWarning;
						default:
							return surprisetalk$elm_bulma$Bulma$Classes$isDanger;
					}
				}(),
					function () {
					switch (size) {
						case 0:
							return surprisetalk$elm_bulma$Bulma$Classes$isSmall;
						case 1:
							return surprisetalk$elm_bulma$Bulma$Classes$none;
						case 2:
							return surprisetalk$elm_bulma$Bulma$Classes$isMedium;
						default:
							return surprisetalk$elm_bulma$Bulma$Classes$isLarge;
					}
				}(),
					function () {
					switch (state) {
						case 0:
							return surprisetalk$elm_bulma$Bulma$Classes$none;
						case 1:
							return surprisetalk$elm_bulma$Bulma$Classes$isHovered;
						case 2:
							return surprisetalk$elm_bulma$Bulma$Classes$isFocused;
						case 3:
							return surprisetalk$elm_bulma$Bulma$Classes$isActive;
						default:
							return surprisetalk$elm_bulma$Bulma$Classes$isLoading;
					}
				}()
				]),
			attrs,
			htmls_);
	});
var surprisetalk$elm_bulma$Bulma$Modifiers$Blur = 0;
var surprisetalk$elm_bulma$Bulma$Elements$buttonModifiers = {b_: 0, ao: false, au: elm$core$Maybe$Nothing, av: elm$core$Maybe$Nothing, aw: false, aD: false, aF: false, q: 1, bC: 0, aI: false};
var surprisetalk$elm_bulma$Bulma$Classes$field = elm$html$Html$Attributes$class('field');
var surprisetalk$elm_bulma$Bulma$Classes$isGrouped = elm$html$Html$Attributes$class('is-grouped');
var surprisetalk$elm_bulma$Bulma$Classes$isGroupedCentered = elm$html$Html$Attributes$class('is-grouped-centered');
var surprisetalk$elm_bulma$Bulma$Classes$isGroupedRight = elm$html$Html$Attributes$class('is-grouped-right');
var surprisetalk$elm_bulma$Bulma$Form$fields = function (alignment) {
	return A2(
		surprisetalk$elm_bulma$Helpers$node,
		'div',
		_List_fromArray(
			[
				surprisetalk$elm_bulma$Bulma$Classes$field,
				surprisetalk$elm_bulma$Bulma$Classes$isGrouped,
				function () {
				switch (alignment) {
					case 0:
						return surprisetalk$elm_bulma$Bulma$Classes$none;
					case 1:
						return surprisetalk$elm_bulma$Bulma$Classes$isGroupedCentered;
					default:
						return surprisetalk$elm_bulma$Bulma$Classes$isGroupedRight;
				}
			}()
			]));
};
var surprisetalk$elm_bulma$Bulma$Modifiers$Dark = 3;
var surprisetalk$elm_bulma$Bulma$Modifiers$Left = 0;
var surprisetalk$elm_bulma$Bulma$Modifiers$Top = 0;
var surprisetalk$elm_bulma$Bulma$Modifiers$Warning = 9;
var surprisetalk$elm_bulma$Bulma$Modifiers$Typography$Bold = 3;
var surprisetalk$elm_bulma$Bulma$Modifiers$Typography$textWeight = function (weight) {
	return elm$html$Html$Attributes$class(
		function () {
			switch (weight) {
				case 0:
					return 'has-text-weight-light';
				case 1:
					return 'has-text-weight-normal';
				case 2:
					return 'has-text-weight-semibold';
				default:
					return 'has-text-weight-bold';
			}
		}());
};
var author$project$Page$viewNavbar = F3(
	function (isMenuOpen, toggle, maybeViewer) {
		var navbarStart = A2(
			surprisetalk$elm_bulma$Bulma$Components$navbarStart,
			_List_Nil,
			_List_fromArray(
				[
					A3(
					surprisetalk$elm_bulma$Bulma$Components$navbarItemLink,
					false,
					_List_fromArray(
						[
							author$project$Route$href(author$project$Route$Dashboard)
						]),
					_List_fromArray(
						[
							elm$html$Html$text('Dashboard')
						])),
					A3(
					surprisetalk$elm_bulma$Bulma$Components$navbarItemLink,
					false,
					_List_fromArray(
						[
							author$project$Route$href(author$project$Route$Upload)
						]),
					_List_fromArray(
						[
							elm$html$Html$text('Upload')
						])),
					A3(
					surprisetalk$elm_bulma$Bulma$Components$navbarItemLink,
					false,
					_List_fromArray(
						[
							author$project$Route$href(author$project$Route$Portfolio)
						]),
					_List_fromArray(
						[
							elm$html$Html$text('Portfolio')
						])),
					A3(
					surprisetalk$elm_bulma$Bulma$Components$navbarItemLink,
					false,
					_List_fromArray(
						[
							author$project$Route$href(author$project$Route$Charts)
						]),
					_List_fromArray(
						[
							elm$html$Html$text('Charts')
						])),
					A3(
					surprisetalk$elm_bulma$Bulma$Components$navbarItemLink,
					false,
					_List_fromArray(
						[
							author$project$Route$href(author$project$Route$AccountBalance)
						]),
					_List_fromArray(
						[
							elm$html$Html$text('Account balance')
						])),
					A3(
					surprisetalk$elm_bulma$Bulma$Components$navbarItemLink,
					false,
					_List_fromArray(
						[
							author$project$Route$href(author$project$Route$ApiDocument)
						]),
					_List_fromArray(
						[
							elm$html$Html$text('API document')
						]))
				]));
		var navbarBrand = A3(
			surprisetalk$elm_bulma$Bulma$Components$navbarBrand,
			_List_Nil,
			A3(
				surprisetalk$elm_bulma$Bulma$Components$navbarBurger,
				isMenuOpen,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						elm$html$Html$div,
						_List_fromArray(
							[
								author$project$Page$stopPropagationOnClick(toggle)
							]),
						_List_fromArray(
							[
								A2(elm$html$Html$span, _List_Nil, _List_Nil),
								A2(elm$html$Html$span, _List_Nil, _List_Nil),
								A2(elm$html$Html$span, _List_Nil, _List_Nil),
								A2(elm$html$Html$span, _List_Nil, _List_Nil),
								A2(elm$html$Html$span, _List_Nil, _List_Nil),
								A2(elm$html$Html$span, _List_Nil, _List_Nil),
								A2(elm$html$Html$span, _List_Nil, _List_Nil)
							]))
					])),
			_List_fromArray(
				[
					A3(
					surprisetalk$elm_bulma$Bulma$Components$navbarItemLink,
					false,
					_List_fromArray(
						[
							author$project$Route$href(author$project$Route$Dashboard),
							surprisetalk$elm_bulma$Bulma$Modifiers$Typography$textSize(4),
							surprisetalk$elm_bulma$Bulma$Modifiers$Typography$textColor(1),
							A2(elm$html$Html$Attributes$style, 'font-family', '\'Gugi\', cursive')
						]),
					_List_fromArray(
						[
							elm$html$Html$text('TRACTOR')
						]))
				]));
		var modi = surprisetalk$elm_bulma$Bulma$Components$navbarModifiers;
		var iconButton = F3(
			function (iconName, color, caption) {
				var icon = A3(
					surprisetalk$elm_bulma$Bulma$Elements$icon,
					1,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							elm$html$Html$i,
							_List_fromArray(
								[
									elm$html$Html$Attributes$class(iconName)
								]),
							_List_Nil)
						]));
				var button = surprisetalk$elm_bulma$Bulma$Elements$button(
					function (x) {
						return _Utils_update(
							x,
							{b_: color});
					}(surprisetalk$elm_bulma$Bulma$Elements$buttonModifiers));
				return A2(
					button,
					_List_fromArray(
						[
							surprisetalk$elm_bulma$Bulma$Modifiers$Typography$textWeight(3)
						]),
					_List_fromArray(
						[
							icon,
							A2(
							elm$html$Html$span,
							_List_Nil,
							_List_fromArray(
								[
									elm$html$Html$text(caption)
								]))
						]));
			});
		var alternateButton = function () {
			var _n0 = function () {
				if (maybeViewer.$ === 1) {
					return _Utils_Tuple2(
						author$project$Route$Login(elm$core$Maybe$Nothing),
						A3(iconButton, 'fas fa-sign-in-alt', 9, 'Log in'));
				} else {
					return _Utils_Tuple2(
						author$project$Route$Logout,
						A3(iconButton, 'fas fa-sign-out-alt', 3, 'Log out'));
				}
			}();
			var route = _n0.a;
			var button = _n0.b;
			return A3(
				surprisetalk$elm_bulma$Bulma$Components$navbarItemLink,
				false,
				_List_fromArray(
					[
						author$project$Route$href(route)
					]),
				_List_fromArray(
					[button]));
		}();
		var navbarEnd = A2(
			surprisetalk$elm_bulma$Bulma$Components$navbarEnd,
			_List_Nil,
			_List_fromArray(
				[
					A3(
					surprisetalk$elm_bulma$Bulma$Components$navbarItem,
					false,
					_List_Nil,
					_List_fromArray(
						[
							A3(
							surprisetalk$elm_bulma$Bulma$Form$fields,
							0,
							_List_Nil,
							_List_fromArray(
								[alternateButton]))
						]))
				]));
		var navbarMenu = A3(
			surprisetalk$elm_bulma$Bulma$Components$navbarMenu,
			isMenuOpen,
			_List_Nil,
			_List_fromArray(
				[navbarStart, navbarEnd]));
		return A4(
			surprisetalk$elm_bulma$Bulma$Components$fixedNavbar,
			0,
			modi,
			_List_fromArray(
				[
					A2(elm$html$Html$Attributes$style, 'border-bottom', 'solid thin gainsboro')
				]),
			_List_fromArray(
				[navbarBrand, navbarMenu]));
	});
var author$project$Page$view = F4(
	function (isMenuOpen, toggle, maybeViewer, _n0) {
		var title = _n0.bG;
		var content = _n0.b$;
		return {
			f: _List_fromArray(
				[
					A3(author$project$Page$viewNavbar, isMenuOpen, toggle, maybeViewer),
					author$project$Page$viewHero(title),
					content,
					author$project$Page$viewFooter
				]),
			bG: title + ' - TRACTOR'
		};
	});
var author$project$Page$AccountBalance$ToggleMenuOpen = {$: 2};
var author$project$Page$AccountBalance$viewReports = function (model) {
	return _List_Nil;
};
var surprisetalk$elm_bulma$Bulma$Layout$NotSpaced = 0;
var surprisetalk$elm_bulma$Bulma$Classes$section = elm$html$Html$Attributes$class('section');
var surprisetalk$elm_bulma$Bulma$Layout$section = function (spacing) {
	return A2(
		surprisetalk$elm_bulma$Helpers$node,
		'section',
		_List_fromArray(
			[
				surprisetalk$elm_bulma$Bulma$Classes$section,
				function () {
				switch (spacing) {
					case 0:
						return surprisetalk$elm_bulma$Bulma$Classes$none;
					case 1:
						return surprisetalk$elm_bulma$Bulma$Classes$isMedium;
					default:
						return surprisetalk$elm_bulma$Bulma$Classes$isLarge;
				}
			}()
			]));
};
var author$project$Page$AccountBalance$view = function (model) {
	return {
		b$: A3(
			surprisetalk$elm_bulma$Bulma$Layout$section,
			0,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					surprisetalk$elm_bulma$Bulma$Layout$container,
					_List_Nil,
					author$project$Page$AccountBalance$viewReports(model))
				])),
		bG: 'Account Balance'
	};
};
var author$project$Page$ApiDocument$ToggleMenuOpen = {$: 2};
var elm$html$Html$p = _VirtualDom_node('p');
var elm_explorations$markdown$Markdown$defaultOptions = {
	a$: elm$core$Maybe$Nothing,
	a9: elm$core$Maybe$Just(
		{bW: false, cq: false}),
	bx: true,
	bA: false
};
var elm_explorations$markdown$Markdown$toHtmlWith = _Markdown_toHtml;
var elm_explorations$markdown$Markdown$toHtml = elm_explorations$markdown$Markdown$toHtmlWith(elm_explorations$markdown$Markdown$defaultOptions);
var author$project$Page$ApiDocument$view = function (model) {
	return {
		b$: function () {
			var _n0 = model.ag;
			if (_n0.$ === 1) {
				return A2(elm$html$Html$p, _List_Nil, _List_Nil);
			} else {
				var doc = _n0.a;
				return A3(
					surprisetalk$elm_bulma$Bulma$Layout$section,
					0,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							surprisetalk$elm_bulma$Bulma$Layout$container,
							_List_Nil,
							_List_fromArray(
								[
									A2(
									elm_explorations$markdown$Markdown$toHtml,
									_List_fromArray(
										[
											elm$html$Html$Attributes$class('content')
										]),
									doc)
								]))
						]));
			}
		}(),
		bG: 'API document'
	};
};
var author$project$Page$Blank$view = {
	b$: elm$html$Html$text(''),
	bG: ''
};
var author$project$Page$Charts$ToggleMenuOpen = {$: 2};
var author$project$Page$Charts$viewCharts = function (model) {
	return _List_Nil;
};
var author$project$Page$Charts$view = function (model) {
	return {
		b$: A3(
			surprisetalk$elm_bulma$Bulma$Layout$section,
			0,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					surprisetalk$elm_bulma$Bulma$Layout$container,
					_List_Nil,
					author$project$Page$Charts$viewCharts(model))
				])),
		bG: 'Charts'
	};
};
var author$project$Page$Dashboard$ToggleMenuOpen = {$: 2};
var author$project$Session$viewer = function (session) {
	if (!session.$) {
		var val = session.b;
		return elm$core$Maybe$Just(val);
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var author$project$Username$toString = function (_n0) {
	var username = _n0;
	return username;
};
var author$project$Api$username = function (_n0) {
	var val = _n0.a;
	return val;
};
var author$project$Viewer$username = function (_n0) {
	var val = _n0;
	return author$project$Api$username(val);
};
var surprisetalk$elm_bulma$Bulma$Classes$notification = elm$html$Html$Attributes$class('notification');
var surprisetalk$elm_bulma$Bulma$Elements$notification = function (color) {
	return A2(
		surprisetalk$elm_bulma$Helpers$node,
		'div',
		_List_fromArray(
			[
				surprisetalk$elm_bulma$Bulma$Classes$notification,
				function () {
				switch (color) {
					case 0:
						return surprisetalk$elm_bulma$Bulma$Classes$none;
					case 1:
						return surprisetalk$elm_bulma$Bulma$Classes$isWhite;
					case 2:
						return surprisetalk$elm_bulma$Bulma$Classes$isLight;
					case 3:
						return surprisetalk$elm_bulma$Bulma$Classes$isDark;
					case 4:
						return surprisetalk$elm_bulma$Bulma$Classes$isBlack;
					case 5:
						return surprisetalk$elm_bulma$Bulma$Classes$isPrimary;
					case 7:
						return surprisetalk$elm_bulma$Bulma$Classes$isInfo;
					case 8:
						return surprisetalk$elm_bulma$Bulma$Classes$isSuccess;
					case 9:
						return surprisetalk$elm_bulma$Bulma$Classes$isWarning;
					case 10:
						return surprisetalk$elm_bulma$Bulma$Classes$isDanger;
					default:
						return surprisetalk$elm_bulma$Bulma$Classes$isLink;
				}
			}()
			]));
};
var surprisetalk$elm_bulma$Bulma$Classes$hasTextCentered = elm$html$Html$Attributes$class('has-text-centered');
var surprisetalk$elm_bulma$Bulma$Modifiers$Typography$textCentered = surprisetalk$elm_bulma$Bulma$Classes$hasTextCentered;
var author$project$Page$Dashboard$viewGreetings = function (model) {
	var username = A2(
		elm$core$Maybe$withDefault,
		'Guest',
		A2(
			elm$core$Maybe$map,
			author$project$Username$toString,
			A2(
				elm$core$Maybe$map,
				author$project$Viewer$username,
				author$project$Session$viewer(model.ac))));
	var message = A2(
		elm$html$Html$p,
		_List_fromArray(
			[
				surprisetalk$elm_bulma$Bulma$Modifiers$Typography$textSize(4)
			]),
		_List_fromArray(
			[
				elm$html$Html$text('Maido!'),
				A2(
				elm$html$Html$span,
				_List_fromArray(
					[
						A2(elm$html$Html$Attributes$style, 'margin', '0 1.8rem')
					]),
				_List_fromArray(
					[
						elm$html$Html$text(username)
					])),
				elm$html$Html$text('san.')
			]));
	return A3(
		surprisetalk$elm_bulma$Bulma$Elements$notification,
		3,
		_List_fromArray(
			[surprisetalk$elm_bulma$Bulma$Modifiers$Typography$textCentered]),
		_List_fromArray(
			[message]));
};
var elm$core$Tuple$second = function (_n0) {
	var y = _n0.b;
	return y;
};
var surprisetalk$elm_bulma$Bulma$Classes$hasAddons = elm$html$Html$Attributes$class('has-addons');
var surprisetalk$elm_bulma$Bulma$Classes$tags = elm$html$Html$Attributes$class('tags');
var surprisetalk$elm_bulma$Bulma$Elements$multitag = A2(
	surprisetalk$elm_bulma$Helpers$node,
	'div',
	_List_fromArray(
		[surprisetalk$elm_bulma$Bulma$Classes$tags, surprisetalk$elm_bulma$Bulma$Classes$hasAddons]));
var surprisetalk$elm_bulma$Bulma$Classes$tag = elm$html$Html$Attributes$class('tag');
var surprisetalk$elm_bulma$Bulma$Elements$tag = function (_n0) {
	var size = _n0.q;
	var color = _n0.b_;
	var isLink = _n0.ax;
	return A2(
		surprisetalk$elm_bulma$Helpers$node,
		isLink ? 'a' : 'span',
		_List_fromArray(
			[
				surprisetalk$elm_bulma$Bulma$Classes$tag,
				function () {
				switch (size) {
					case 0:
						return surprisetalk$elm_bulma$Bulma$Classes$none;
					case 1:
						return surprisetalk$elm_bulma$Bulma$Classes$none;
					case 2:
						return surprisetalk$elm_bulma$Bulma$Classes$isMedium;
					default:
						return surprisetalk$elm_bulma$Bulma$Classes$isLarge;
				}
			}(),
				function () {
				switch (color) {
					case 0:
						return surprisetalk$elm_bulma$Bulma$Classes$none;
					case 1:
						return surprisetalk$elm_bulma$Bulma$Classes$isWhite;
					case 2:
						return surprisetalk$elm_bulma$Bulma$Classes$isLight;
					case 3:
						return surprisetalk$elm_bulma$Bulma$Classes$isDark;
					case 4:
						return surprisetalk$elm_bulma$Bulma$Classes$isBlack;
					case 5:
						return surprisetalk$elm_bulma$Bulma$Classes$isPrimary;
					case 7:
						return surprisetalk$elm_bulma$Bulma$Classes$isInfo;
					case 8:
						return surprisetalk$elm_bulma$Bulma$Classes$isSuccess;
					case 9:
						return surprisetalk$elm_bulma$Bulma$Classes$isWarning;
					case 10:
						return surprisetalk$elm_bulma$Bulma$Classes$isDanger;
					default:
						return surprisetalk$elm_bulma$Bulma$Classes$isLink;
				}
			}()
			]));
};
var surprisetalk$elm_bulma$Bulma$Elements$tagModifiers = {b_: 0, ax: false, q: 1};
var author$project$Page$Dashboard$badge = F2(
	function (colors, texts) {
		var tag = F2(
			function (col, txt) {
				return A3(
					surprisetalk$elm_bulma$Bulma$Elements$tag,
					_Utils_update(
						surprisetalk$elm_bulma$Bulma$Elements$tagModifiers,
						{b_: col}),
					_List_Nil,
					_List_fromArray(
						[
							elm$html$Html$text(txt)
						]));
			});
		return A2(
			surprisetalk$elm_bulma$Bulma$Elements$multitag,
			_List_Nil,
			_List_fromArray(
				[
					A2(tag, colors.a, texts.a),
					A2(tag, colors.b, texts.b)
				]));
	});
var elm$core$Result$withDefault = F2(
	function (def, result) {
		if (!result.$) {
			var a = result.a;
			return a;
		} else {
			return def;
		}
	});
var surprisetalk$elm_bulma$Bulma$Classes$control = elm$html$Html$Attributes$class('control');
var surprisetalk$elm_bulma$Bulma$Classes$hasIconsLeft = elm$html$Html$Attributes$class('has-icons-left');
var surprisetalk$elm_bulma$Bulma$Classes$hasIconsRight = elm$html$Html$Attributes$class('has-icons-right');
var surprisetalk$elm_bulma$Bulma$Classes$isExpanded = elm$html$Html$Attributes$class('is-expanded');
var surprisetalk$elm_bulma$Bulma$Form$control = F3(
	function (_n0, attrs, htmls) {
		var loading = _n0.C;
		var expanded = _n0.h;
		var iconLeft = _n0.au;
		var iconRight = _n0.av;
		return A4(
			surprisetalk$elm_bulma$Helpers$node,
			'p',
			_List_fromArray(
				[
					surprisetalk$elm_bulma$Bulma$Classes$control,
					function () {
					if (!loading.$) {
						return surprisetalk$elm_bulma$Bulma$Classes$isLoading;
					} else {
						return surprisetalk$elm_bulma$Bulma$Classes$none;
					}
				}(),
					function () {
					_n2$3:
					while (true) {
						if (!loading.$) {
							switch (loading.a) {
								case 0:
									var _n3 = loading.a;
									return surprisetalk$elm_bulma$Bulma$Classes$isSmall;
								case 2:
									var _n4 = loading.a;
									return surprisetalk$elm_bulma$Bulma$Classes$isMedium;
								case 3:
									var _n5 = loading.a;
									return surprisetalk$elm_bulma$Bulma$Classes$isLarge;
								default:
									break _n2$3;
							}
						} else {
							break _n2$3;
						}
					}
					return surprisetalk$elm_bulma$Bulma$Classes$none;
				}(),
					function () {
					if (expanded) {
						return surprisetalk$elm_bulma$Bulma$Classes$isExpanded;
					} else {
						return surprisetalk$elm_bulma$Bulma$Classes$none;
					}
				}(),
					function () {
					if (!iconLeft.$) {
						return surprisetalk$elm_bulma$Bulma$Classes$hasIconsLeft;
					} else {
						return surprisetalk$elm_bulma$Bulma$Classes$none;
					}
				}(),
					function () {
					if (!iconRight.$) {
						return surprisetalk$elm_bulma$Bulma$Classes$hasIconsRight;
					} else {
						return surprisetalk$elm_bulma$Bulma$Classes$none;
					}
				}()
				]),
			attrs,
			_Utils_ap(
				htmls,
				_Utils_ap(
					A2(
						elm$core$Maybe$withDefault,
						_List_Nil,
						A2(
							elm$core$Maybe$map,
							function (_n9) {
								var size_ = _n9.a;
								var attrs_ = _n9.b;
								var iconBody = _n9.c;
								return _List_fromArray(
									[
										A3(
										surprisetalk$elm_bulma$Bulma$Elements$icon,
										size_,
										A2(
											elm$core$List$cons,
											elm$html$Html$Attributes$class('is-left'),
											attrs_),
										_List_fromArray(
											[iconBody]))
									]);
							},
							iconLeft)),
					_Utils_ap(
						A2(
							elm$core$Maybe$withDefault,
							_List_Nil,
							A2(
								elm$core$Maybe$map,
								function (_n10) {
									var size_ = _n10.a;
									var attrs_ = _n10.b;
									var iconBody = _n10.c;
									return _List_fromArray(
										[
											A3(
											surprisetalk$elm_bulma$Bulma$Elements$icon,
											size_,
											A2(
												elm$core$List$cons,
												elm$html$Html$Attributes$class('is-right'),
												attrs_),
											_List_fromArray(
												[iconBody]))
										]);
								},
								iconRight)),
						_List_Nil))));
	});
var surprisetalk$elm_bulma$Bulma$Form$controlModifiers = {h: false, au: elm$core$Maybe$Nothing, av: elm$core$Maybe$Nothing, C: elm$core$Maybe$Nothing};
var surprisetalk$elm_bulma$Bulma$Classes$isGroupedMultiline = elm$html$Html$Attributes$class('is-grouped-multiline');
var surprisetalk$elm_bulma$Bulma$Form$multilineFields = A2(
	surprisetalk$elm_bulma$Helpers$node,
	'div',
	_List_fromArray(
		[surprisetalk$elm_bulma$Bulma$Classes$field, surprisetalk$elm_bulma$Bulma$Classes$isGrouped, surprisetalk$elm_bulma$Bulma$Classes$isGroupedMultiline]));
var surprisetalk$elm_bulma$Bulma$Modifiers$Danger = 10;
var surprisetalk$elm_bulma$Bulma$Modifiers$Info = 7;
var surprisetalk$elm_bulma$Bulma$Modifiers$Success = 8;
var author$project$Page$Dashboard$viewStatusLine = function (model) {
	var maybeToList = function (m) {
		if (m.$ === 1) {
			return _List_Nil;
		} else {
			var x = m.a;
			return _List_fromArray(
				[x]);
		}
	};
	var infoBadge = author$project$Page$Dashboard$badge(
		_Utils_Tuple2(3, 7));
	var osBadge = function (v) {
		return infoBadge(
			_Utils_Tuple2('OS', v.aS));
	};
	var versionBadge = function (v) {
		return infoBadge(
			_Utils_Tuple2('Version', v.bO));
	};
	var gitReposBadge = function (v) {
		return infoBadge(
			_Utils_Tuple2('Repos', v.a8));
	};
	var gitCommitsBadge = function (v) {
		return infoBadge(
			_Utils_Tuple2('Commits', v.a5));
	};
	var control = A2(surprisetalk$elm_bulma$Bulma$Form$control, surprisetalk$elm_bulma$Bulma$Form$controlModifiers, _List_Nil);
	var failureBadge = control(
		_List_fromArray(
			[
				A2(
				author$project$Page$Dashboard$badge,
				_Utils_Tuple2(3, 10),
				_Utils_Tuple2('System', 'failure'))
			]));
	var greenBadge = control(
		_List_fromArray(
			[
				A2(
				author$project$Page$Dashboard$badge,
				_Utils_Tuple2(3, 8),
				_Utils_Tuple2('System', 'green'))
			]));
	var haltedBadge = control(
		_List_fromArray(
			[
				A2(
				author$project$Page$Dashboard$badge,
				_Utils_Tuple2(3, 10),
				_Utils_Tuple2('System', 'halted'))
			]));
	var warningBadge = control(
		_List_fromArray(
			[
				A2(
				author$project$Page$Dashboard$badge,
				_Utils_Tuple2(3, 9),
				_Utils_Tuple2('System', 'warning'))
			]));
	var healthBadge = A2(
		elm$core$Basics$composeL,
		elm$core$Result$withDefault(haltedBadge),
		elm$core$Result$map(
			function (x) {
				var _n1 = x.cp;
				switch (_n1) {
					case 'Green':
						return greenBadge;
					case 'Yellow':
						return warningBadge;
					default:
						return failureBadge;
				}
			}));
	var archBadge = function (v) {
		return infoBadge(
			_Utils_Tuple2('Arch', v.aR));
	};
	var vtags = function (maybeSV) {
		if (maybeSV.$ === 1) {
			return _List_Nil;
		} else {
			var sv = maybeSV.a;
			return _List_fromArray(
				[
					control(
					_List_fromArray(
						[
							versionBadge(sv)
						])),
					control(
					_List_fromArray(
						[
							archBadge(sv)
						])),
					control(
					_List_fromArray(
						[
							osBadge(sv)
						])),
					control(
					_List_fromArray(
						[
							gitReposBadge(sv)
						])),
					control(
					_List_fromArray(
						[
							gitCommitsBadge(sv)
						]))
				]);
		}
	};
	return A2(
		surprisetalk$elm_bulma$Bulma$Form$multilineFields,
		_List_Nil,
		elm$core$List$concat(
			_List_fromArray(
				[
					A2(
					elm$core$List$map,
					healthBadge,
					maybeToList(model.ad)),
					vtags(model.ae)
				])));
};
var author$project$Page$Dashboard$viewDashboard = function (model) {
	return _List_fromArray(
		[
			author$project$Page$Dashboard$viewStatusLine(model),
			author$project$Page$Dashboard$viewGreetings(model)
		]);
};
var author$project$Page$Dashboard$view = function (model) {
	return {
		b$: A3(
			surprisetalk$elm_bulma$Bulma$Layout$section,
			0,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					surprisetalk$elm_bulma$Bulma$Layout$container,
					_List_Nil,
					author$project$Page$Dashboard$viewDashboard(model))
				])),
		bG: 'Dashboard'
	};
};
var author$project$Page$Login$ToggleMenuOpen = {$: 1};
var surprisetalk$elm_bulma$Bulma$Modifiers$Width4 = 4;
var author$project$Page$viewErrors = function (errors) {
	var notification = A3(
		surprisetalk$elm_bulma$Bulma$Elements$notification,
		10,
		_List_Nil,
		A2(
			elm$core$List$map,
			function (x) {
				return A2(
					elm$html$Html$p,
					_List_Nil,
					_List_fromArray(
						[
							elm$html$Html$text(x)
						]));
			},
			errors));
	return A3(
		surprisetalk$elm_bulma$Bulma$Layout$tileAncestor,
		0,
		_List_fromArray(
			[
				surprisetalk$elm_bulma$Bulma$Modifiers$Typography$textSize(4),
				surprisetalk$elm_bulma$Bulma$Modifiers$Typography$textCentered
			]),
		_List_fromArray(
			[
				A3(surprisetalk$elm_bulma$Bulma$Layout$verticalTile, 0, _List_Nil, _List_Nil),
				A3(
				surprisetalk$elm_bulma$Bulma$Layout$verticalTile,
				4,
				_List_Nil,
				_List_fromArray(
					[notification])),
				A3(surprisetalk$elm_bulma$Bulma$Layout$verticalTile, 0, _List_Nil, _List_Nil)
			]));
};
var elm$url$Url$Builder$toQueryPair = function (_n0) {
	var key = _n0.a;
	var value = _n0.b;
	return key + ('=' + value);
};
var elm$url$Url$Builder$toQuery = function (parameters) {
	if (!parameters.b) {
		return '';
	} else {
		return '?' + A2(
			elm$core$String$join,
			'&',
			A2(elm$core$List$map, elm$url$Url$Builder$toQueryPair, parameters));
	}
};
var elm$url$Url$Builder$crossOrigin = F3(
	function (prePath, pathSegments, parameters) {
		return prePath + ('/' + (A2(elm$core$String$join, '/', pathSegments) + elm$url$Url$Builder$toQuery(parameters)));
	});
var elm$url$Url$percentEncode = _Url_percentEncode;
var elm$url$Url$Builder$QueryParameter = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var elm$url$Url$Builder$string = F2(
	function (key, value) {
		return A2(
			elm$url$Url$Builder$QueryParameter,
			elm$url$Url$percentEncode(key),
			elm$url$Url$percentEncode(value));
	});
var author$project$Page$Login$signInWithSlackButton = function (authClientId) {
	var slackMark = A2(
		elm$html$Html$img,
		_List_fromArray(
			[
				A2(elm$html$Html$Attributes$style, 'vertical-align', 'middle'),
				elm$html$Html$Attributes$src('public/assets/Slack_Monochrome_White.svg'),
				elm$html$Html$Attributes$alt('Slack'),
				elm$html$Html$Attributes$width(80)
			]),
		_List_Nil);
	var modi = function (x) {
		return _Utils_update(
			x,
			{b_: 3});
	}(surprisetalk$elm_bulma$Bulma$Elements$buttonModifiers);
	var icon = A3(
		surprisetalk$elm_bulma$Bulma$Elements$icon,
		1,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				elm$html$Html$i,
				_List_fromArray(
					[
						elm$html$Html$Attributes$class('fab fa-slack')
					]),
				_List_Nil)
			]));
	var hrefSignInWithSlack = A3(
		elm$url$Url$Builder$crossOrigin,
		'https://slack.com',
		_List_fromArray(
			['oauth', 'authorize']),
		_List_fromArray(
			[
				A2(elm$url$Url$Builder$string, 'scope', 'identity.basic'),
				A2(elm$url$Url$Builder$string, 'client_id', authClientId.aW),
				A2(elm$url$Url$Builder$string, 'state', 'abcdefg')
			]));
	return A2(
		elm$html$Html$p,
		_List_fromArray(
			[surprisetalk$elm_bulma$Bulma$Modifiers$Typography$textCentered]),
		_List_fromArray(
			[
				A2(
				elm$html$Html$a,
				_List_fromArray(
					[
						elm$html$Html$Attributes$href(hrefSignInWithSlack)
					]),
				_List_fromArray(
					[
						A3(
						surprisetalk$elm_bulma$Bulma$Elements$button,
						modi,
						_List_fromArray(
							[
								surprisetalk$elm_bulma$Bulma$Modifiers$Typography$textSize(4)
							]),
						_List_fromArray(
							[
								A2(
								elm$html$Html$div,
								_List_fromArray(
									[
										A2(elm$html$Html$Attributes$style, 'vertical-align', 'middle'),
										A2(elm$html$Html$Attributes$style, 'display', 'inline-block')
									]),
								_List_fromArray(
									[
										slackMark,
										elm$html$Html$text('アカウントでログイン')
									]))
							]))
					]))
			]));
};
var author$project$Page$Login$viewContents = function (authClientId) {
	return _List_fromArray(
		[
			A2(
			elm$html$Html$p,
			_List_Nil,
			_List_fromArray(
				[
					author$project$Page$Login$signInWithSlackButton(authClientId)
				]))
		]);
};
var author$project$Page$Login$view = function (model) {
	var concreteView = function () {
		var _n0 = model.J;
		switch (_n0.$) {
			case 0:
				var stage = _n0.a;
				return A2(
					elm$core$Maybe$withDefault,
					_List_Nil,
					A2(elm$core$Maybe$map, author$project$Page$Login$viewContents, stage.ai));
			case 1:
				var stage = _n0.a;
				return _List_fromArray(
					[
						elm$html$Html$text('Success to Login')
					]);
			default:
				var stage = _n0.a;
				return _List_fromArray(
					[
						author$project$Page$viewErrors(
						_List_fromArray(
							['Auth error', stage.al]))
					]);
		}
	}();
	return {
		b$: A3(
			surprisetalk$elm_bulma$Bulma$Layout$section,
			0,
			_List_Nil,
			_List_fromArray(
				[
					A2(surprisetalk$elm_bulma$Bulma$Layout$container, _List_Nil, concreteView)
				])),
		bG: 'Log in'
	};
};
var elm$html$Html$h1 = _VirtualDom_node('h1');
var elm$html$Html$main_ = _VirtualDom_node('main');
var elm$html$Html$Attributes$id = elm$html$Html$Attributes$stringProperty('id');
var elm$html$Html$Attributes$tabindex = function (n) {
	return A2(
		_VirtualDom_attribute,
		'tabIndex',
		elm$core$String$fromInt(n));
};
var author$project$Page$NotFound$view = {
	b$: A2(
		elm$html$Html$main_,
		_List_fromArray(
			[
				elm$html$Html$Attributes$id('content'),
				elm$html$Html$Attributes$class('container'),
				elm$html$Html$Attributes$tabindex(-1)
			]),
		_List_fromArray(
			[
				A2(
				elm$html$Html$h1,
				_List_Nil,
				_List_fromArray(
					[
						elm$html$Html$text('Not Found')
					]))
			])),
	bG: 'Page Not Found'
};
var author$project$Page$Portfolio$ToggleMenuOpen = {$: 2};
var author$project$Page$Portfolio$viewReports = function (model) {
	return _List_Nil;
};
var author$project$Page$Portfolio$view = function (model) {
	return {
		b$: A3(
			surprisetalk$elm_bulma$Bulma$Layout$section,
			0,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					surprisetalk$elm_bulma$Bulma$Layout$container,
					_List_Nil,
					author$project$Page$Portfolio$viewReports(model))
				])),
		bG: 'Portfolio'
	};
};
var author$project$Page$Upload$ToggleMenuOpen = {$: 2};
var author$project$Page$Upload$NoOp = {$: 0};
var surprisetalk$elm_bulma$Bulma$Columns$columnModifiers = {
	cj: 0,
	bS: {
		b0: elm$core$Maybe$Just(0),
		b9: elm$core$Maybe$Just(0),
		aA: elm$core$Maybe$Just(0),
		cr: elm$core$Maybe$Just(0),
		cu: elm$core$Maybe$Just(0)
	}
};
var author$project$Page$Upload$myColumnModifiers = F2(
	function (offset, width) {
		var widths = surprisetalk$elm_bulma$Bulma$Columns$columnModifiers.bS;
		return _Utils_update(
			surprisetalk$elm_bulma$Bulma$Columns$columnModifiers,
			{
				cj: offset,
				bS: _Utils_update(
					widths,
					{b0: width, b9: width, cr: width, cu: width})
			});
	});
var elm$html$Html$strong = _VirtualDom_node('strong');
var surprisetalk$elm_bulma$Bulma$Classes$column = elm$html$Html$Attributes$class('column');
var surprisetalk$elm_bulma$Bulma$Classes$is01Desktop = elm$html$Html$Attributes$class('is-1-desktop');
var surprisetalk$elm_bulma$Bulma$Classes$is01FullHD = elm$html$Html$Attributes$class('is-1-fullhd');
var surprisetalk$elm_bulma$Bulma$Classes$is01Mobile = elm$html$Html$Attributes$class('is-1-mobile');
var surprisetalk$elm_bulma$Bulma$Classes$is01Tablet = elm$html$Html$Attributes$class('is-1-tablet');
var surprisetalk$elm_bulma$Bulma$Classes$is01Widescreen = elm$html$Html$Attributes$class('is-1-widescreen');
var surprisetalk$elm_bulma$Bulma$Classes$is02Desktop = elm$html$Html$Attributes$class('is-2-desktop');
var surprisetalk$elm_bulma$Bulma$Classes$is02FullHD = elm$html$Html$Attributes$class('is-2-fullhd');
var surprisetalk$elm_bulma$Bulma$Classes$is02Mobile = elm$html$Html$Attributes$class('is-2-mobile');
var surprisetalk$elm_bulma$Bulma$Classes$is02Tablet = elm$html$Html$Attributes$class('is-2-tablet');
var surprisetalk$elm_bulma$Bulma$Classes$is02Widescreen = elm$html$Html$Attributes$class('is-2-widescreen');
var surprisetalk$elm_bulma$Bulma$Classes$is03Desktop = elm$html$Html$Attributes$class('is-3-desktop');
var surprisetalk$elm_bulma$Bulma$Classes$is03FullHD = elm$html$Html$Attributes$class('is-3-fullhd');
var surprisetalk$elm_bulma$Bulma$Classes$is03Mobile = elm$html$Html$Attributes$class('is-3-mobile');
var surprisetalk$elm_bulma$Bulma$Classes$is03Tablet = elm$html$Html$Attributes$class('is-3-tablet');
var surprisetalk$elm_bulma$Bulma$Classes$is03Widescreen = elm$html$Html$Attributes$class('is-3-widescreen');
var surprisetalk$elm_bulma$Bulma$Classes$is04Desktop = elm$html$Html$Attributes$class('is-4-desktop');
var surprisetalk$elm_bulma$Bulma$Classes$is04FullHD = elm$html$Html$Attributes$class('is-4-fullhd');
var surprisetalk$elm_bulma$Bulma$Classes$is04Mobile = elm$html$Html$Attributes$class('is-4-mobile');
var surprisetalk$elm_bulma$Bulma$Classes$is04Tablet = elm$html$Html$Attributes$class('is-4-tablet');
var surprisetalk$elm_bulma$Bulma$Classes$is04Widescreen = elm$html$Html$Attributes$class('is-4-widescreen');
var surprisetalk$elm_bulma$Bulma$Classes$is05Desktop = elm$html$Html$Attributes$class('is-5-desktop');
var surprisetalk$elm_bulma$Bulma$Classes$is05FullHD = elm$html$Html$Attributes$class('is-5-fullhd');
var surprisetalk$elm_bulma$Bulma$Classes$is05Mobile = elm$html$Html$Attributes$class('is-5-mobile');
var surprisetalk$elm_bulma$Bulma$Classes$is05Tablet = elm$html$Html$Attributes$class('is-5-tablet');
var surprisetalk$elm_bulma$Bulma$Classes$is05Widescreen = elm$html$Html$Attributes$class('is-5-widescreen');
var surprisetalk$elm_bulma$Bulma$Classes$is06Desktop = elm$html$Html$Attributes$class('is-6-desktop');
var surprisetalk$elm_bulma$Bulma$Classes$is06FullHD = elm$html$Html$Attributes$class('is-6-fullhd');
var surprisetalk$elm_bulma$Bulma$Classes$is06Mobile = elm$html$Html$Attributes$class('is-6-mobile');
var surprisetalk$elm_bulma$Bulma$Classes$is06Tablet = elm$html$Html$Attributes$class('is-6-tablet');
var surprisetalk$elm_bulma$Bulma$Classes$is06Widescreen = elm$html$Html$Attributes$class('is-6-widescreen');
var surprisetalk$elm_bulma$Bulma$Classes$is07Desktop = elm$html$Html$Attributes$class('is-7-desktop');
var surprisetalk$elm_bulma$Bulma$Classes$is07FullHD = elm$html$Html$Attributes$class('is-7-fullhd');
var surprisetalk$elm_bulma$Bulma$Classes$is07Mobile = elm$html$Html$Attributes$class('is-7-mobile');
var surprisetalk$elm_bulma$Bulma$Classes$is07Tablet = elm$html$Html$Attributes$class('is-7-tablet');
var surprisetalk$elm_bulma$Bulma$Classes$is07Widescreen = elm$html$Html$Attributes$class('is-7-widescreen');
var surprisetalk$elm_bulma$Bulma$Classes$is08Desktop = elm$html$Html$Attributes$class('is-8-desktop');
var surprisetalk$elm_bulma$Bulma$Classes$is08FullHD = elm$html$Html$Attributes$class('is-8-fullhd');
var surprisetalk$elm_bulma$Bulma$Classes$is08Mobile = elm$html$Html$Attributes$class('is-8-mobile');
var surprisetalk$elm_bulma$Bulma$Classes$is08Tablet = elm$html$Html$Attributes$class('is-8-tablet');
var surprisetalk$elm_bulma$Bulma$Classes$is08Widescreen = elm$html$Html$Attributes$class('is-8-widescreen');
var surprisetalk$elm_bulma$Bulma$Classes$is09Desktop = elm$html$Html$Attributes$class('is-9-desktop');
var surprisetalk$elm_bulma$Bulma$Classes$is09FullHD = elm$html$Html$Attributes$class('is-9-fullhd');
var surprisetalk$elm_bulma$Bulma$Classes$is09Mobile = elm$html$Html$Attributes$class('is-9-mobile');
var surprisetalk$elm_bulma$Bulma$Classes$is09Tablet = elm$html$Html$Attributes$class('is-9-tablet');
var surprisetalk$elm_bulma$Bulma$Classes$is09Widescreen = elm$html$Html$Attributes$class('is-9-widescreen');
var surprisetalk$elm_bulma$Bulma$Classes$is10Desktop = elm$html$Html$Attributes$class('is-10-desktop');
var surprisetalk$elm_bulma$Bulma$Classes$is10FullHD = elm$html$Html$Attributes$class('is-10-fullhd');
var surprisetalk$elm_bulma$Bulma$Classes$is10Mobile = elm$html$Html$Attributes$class('is-10-mobile');
var surprisetalk$elm_bulma$Bulma$Classes$is10Tablet = elm$html$Html$Attributes$class('is-10-tablet');
var surprisetalk$elm_bulma$Bulma$Classes$is10Widescreen = elm$html$Html$Attributes$class('is-10-widescreen');
var surprisetalk$elm_bulma$Bulma$Classes$is11Desktop = elm$html$Html$Attributes$class('is-11-desktop');
var surprisetalk$elm_bulma$Bulma$Classes$is11FullHD = elm$html$Html$Attributes$class('is-11-fullhd');
var surprisetalk$elm_bulma$Bulma$Classes$is11Mobile = elm$html$Html$Attributes$class('is-11-mobile');
var surprisetalk$elm_bulma$Bulma$Classes$is11Tablet = elm$html$Html$Attributes$class('is-11-tablet');
var surprisetalk$elm_bulma$Bulma$Classes$is11Widescreen = elm$html$Html$Attributes$class('is-11-widescreen');
var surprisetalk$elm_bulma$Bulma$Classes$isNarrowDesktop = elm$html$Html$Attributes$class('is-narrow-desktop');
var surprisetalk$elm_bulma$Bulma$Classes$isNarrowFullHD = elm$html$Html$Attributes$class('is-narrow-fullhd');
var surprisetalk$elm_bulma$Bulma$Classes$isNarrowMobile = elm$html$Html$Attributes$class('is-narrow-mobile');
var surprisetalk$elm_bulma$Bulma$Classes$isNarrowTablet = elm$html$Html$Attributes$class('is-narrow-tablet');
var surprisetalk$elm_bulma$Bulma$Classes$isNarrowWidescreen = elm$html$Html$Attributes$class('is-narrow-widescreen');
var surprisetalk$elm_bulma$Bulma$Classes$isOffset01 = elm$html$Html$Attributes$class('is-offset-1');
var surprisetalk$elm_bulma$Bulma$Classes$isOffset02 = elm$html$Html$Attributes$class('is-offset-2');
var surprisetalk$elm_bulma$Bulma$Classes$isOffset03 = elm$html$Html$Attributes$class('is-offset-3');
var surprisetalk$elm_bulma$Bulma$Classes$isOffset04 = elm$html$Html$Attributes$class('is-offset-4');
var surprisetalk$elm_bulma$Bulma$Classes$isOffset05 = elm$html$Html$Attributes$class('is-offset-5');
var surprisetalk$elm_bulma$Bulma$Classes$isOffset06 = elm$html$Html$Attributes$class('is-offset-6');
var surprisetalk$elm_bulma$Bulma$Classes$isOffset07 = elm$html$Html$Attributes$class('is-offset-7');
var surprisetalk$elm_bulma$Bulma$Classes$isOffset08 = elm$html$Html$Attributes$class('is-offset-8');
var surprisetalk$elm_bulma$Bulma$Classes$isOffset09 = elm$html$Html$Attributes$class('is-offset-9');
var surprisetalk$elm_bulma$Bulma$Classes$isOffset10 = elm$html$Html$Attributes$class('is-offset-10');
var surprisetalk$elm_bulma$Bulma$Classes$isOffset11 = elm$html$Html$Attributes$class('is-offset-11');
var surprisetalk$elm_bulma$Bulma$Columns$column = function (_n0) {
	var widths = _n0.bS;
	var offset = _n0.cj;
	return A2(
		surprisetalk$elm_bulma$Helpers$node,
		'div',
		_List_fromArray(
			[
				surprisetalk$elm_bulma$Bulma$Classes$column,
				function () {
				var _n1 = widths.aA;
				if (!_n1.$) {
					switch (_n1.a) {
						case 0:
							var _n2 = _n1.a;
							return surprisetalk$elm_bulma$Bulma$Classes$none;
						case 1:
							var _n3 = _n1.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is01Mobile;
						case 2:
							var _n4 = _n1.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is02Mobile;
						case 3:
							var _n5 = _n1.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is03Mobile;
						case 4:
							var _n6 = _n1.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is04Mobile;
						case 5:
							var _n7 = _n1.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is05Mobile;
						case 6:
							var _n8 = _n1.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is06Mobile;
						case 7:
							var _n9 = _n1.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is07Mobile;
						case 8:
							var _n10 = _n1.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is08Mobile;
						case 9:
							var _n11 = _n1.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is09Mobile;
						case 10:
							var _n12 = _n1.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is10Mobile;
						default:
							var _n13 = _n1.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is11Mobile;
					}
				} else {
					return surprisetalk$elm_bulma$Bulma$Classes$isNarrowMobile;
				}
			}(),
				function () {
				var _n14 = widths.cr;
				if (!_n14.$) {
					switch (_n14.a) {
						case 0:
							var _n15 = _n14.a;
							return surprisetalk$elm_bulma$Bulma$Classes$none;
						case 1:
							var _n16 = _n14.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is01Tablet;
						case 2:
							var _n17 = _n14.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is02Tablet;
						case 3:
							var _n18 = _n14.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is03Tablet;
						case 4:
							var _n19 = _n14.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is04Tablet;
						case 5:
							var _n20 = _n14.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is05Tablet;
						case 6:
							var _n21 = _n14.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is06Tablet;
						case 7:
							var _n22 = _n14.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is07Tablet;
						case 8:
							var _n23 = _n14.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is08Tablet;
						case 9:
							var _n24 = _n14.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is09Tablet;
						case 10:
							var _n25 = _n14.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is10Tablet;
						default:
							var _n26 = _n14.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is11Tablet;
					}
				} else {
					return surprisetalk$elm_bulma$Bulma$Classes$isNarrowTablet;
				}
			}(),
				function () {
				var _n27 = widths.b0;
				if (!_n27.$) {
					switch (_n27.a) {
						case 0:
							var _n28 = _n27.a;
							return surprisetalk$elm_bulma$Bulma$Classes$none;
						case 1:
							var _n29 = _n27.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is01Desktop;
						case 2:
							var _n30 = _n27.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is02Desktop;
						case 3:
							var _n31 = _n27.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is03Desktop;
						case 4:
							var _n32 = _n27.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is04Desktop;
						case 5:
							var _n33 = _n27.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is05Desktop;
						case 6:
							var _n34 = _n27.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is06Desktop;
						case 7:
							var _n35 = _n27.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is07Desktop;
						case 8:
							var _n36 = _n27.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is08Desktop;
						case 9:
							var _n37 = _n27.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is09Desktop;
						case 10:
							var _n38 = _n27.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is10Desktop;
						default:
							var _n39 = _n27.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is11Desktop;
					}
				} else {
					return surprisetalk$elm_bulma$Bulma$Classes$isNarrowDesktop;
				}
			}(),
				function () {
				var _n40 = widths.cu;
				if (!_n40.$) {
					switch (_n40.a) {
						case 0:
							var _n41 = _n40.a;
							return surprisetalk$elm_bulma$Bulma$Classes$none;
						case 1:
							var _n42 = _n40.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is01Widescreen;
						case 2:
							var _n43 = _n40.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is02Widescreen;
						case 3:
							var _n44 = _n40.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is03Widescreen;
						case 4:
							var _n45 = _n40.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is04Widescreen;
						case 5:
							var _n46 = _n40.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is05Widescreen;
						case 6:
							var _n47 = _n40.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is06Widescreen;
						case 7:
							var _n48 = _n40.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is07Widescreen;
						case 8:
							var _n49 = _n40.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is08Widescreen;
						case 9:
							var _n50 = _n40.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is09Widescreen;
						case 10:
							var _n51 = _n40.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is10Widescreen;
						default:
							var _n52 = _n40.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is11Widescreen;
					}
				} else {
					return surprisetalk$elm_bulma$Bulma$Classes$isNarrowWidescreen;
				}
			}(),
				function () {
				var _n53 = widths.b9;
				if (!_n53.$) {
					switch (_n53.a) {
						case 0:
							var _n54 = _n53.a;
							return surprisetalk$elm_bulma$Bulma$Classes$none;
						case 1:
							var _n55 = _n53.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is01FullHD;
						case 2:
							var _n56 = _n53.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is02FullHD;
						case 3:
							var _n57 = _n53.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is03FullHD;
						case 4:
							var _n58 = _n53.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is04FullHD;
						case 5:
							var _n59 = _n53.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is05FullHD;
						case 6:
							var _n60 = _n53.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is06FullHD;
						case 7:
							var _n61 = _n53.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is07FullHD;
						case 8:
							var _n62 = _n53.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is08FullHD;
						case 9:
							var _n63 = _n53.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is09FullHD;
						case 10:
							var _n64 = _n53.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is10FullHD;
						default:
							var _n65 = _n53.a;
							return surprisetalk$elm_bulma$Bulma$Classes$is11FullHD;
					}
				} else {
					return surprisetalk$elm_bulma$Bulma$Classes$isNarrowFullHD;
				}
			}(),
				function () {
				switch (offset) {
					case 0:
						return surprisetalk$elm_bulma$Bulma$Classes$none;
					case 1:
						return surprisetalk$elm_bulma$Bulma$Classes$isOffset01;
					case 2:
						return surprisetalk$elm_bulma$Bulma$Classes$isOffset02;
					case 3:
						return surprisetalk$elm_bulma$Bulma$Classes$isOffset03;
					case 4:
						return surprisetalk$elm_bulma$Bulma$Classes$isOffset04;
					case 5:
						return surprisetalk$elm_bulma$Bulma$Classes$isOffset05;
					case 6:
						return surprisetalk$elm_bulma$Bulma$Classes$isOffset06;
					case 7:
						return surprisetalk$elm_bulma$Bulma$Classes$isOffset07;
					case 8:
						return surprisetalk$elm_bulma$Bulma$Classes$isOffset08;
					case 9:
						return surprisetalk$elm_bulma$Bulma$Classes$isOffset09;
					case 10:
						return surprisetalk$elm_bulma$Bulma$Classes$isOffset10;
					default:
						return surprisetalk$elm_bulma$Bulma$Classes$isOffset11;
				}
			}()
			]));
};
var surprisetalk$elm_bulma$Bulma$Classes$columns = elm$html$Html$Attributes$class('columns');
var surprisetalk$elm_bulma$Bulma$Classes$is0 = elm$html$Html$Attributes$class('is-0');
var surprisetalk$elm_bulma$Bulma$Classes$isCentered = elm$html$Html$Attributes$class('is-centered');
var surprisetalk$elm_bulma$Bulma$Classes$isDesktop = elm$html$Html$Attributes$class('is-desktop');
var surprisetalk$elm_bulma$Bulma$Classes$isGapless = elm$html$Html$Attributes$class('is-gapless');
var surprisetalk$elm_bulma$Bulma$Classes$isMobile = elm$html$Html$Attributes$class('is-mobile');
var surprisetalk$elm_bulma$Bulma$Classes$isMultiline = elm$html$Html$Attributes$class('is-multiline');
var surprisetalk$elm_bulma$Bulma$Columns$columns = function (_n0) {
	var centered = _n0.am;
	var multiline = _n0.bj;
	var gap = _n0.as;
	var display = _n0.ap;
	return A2(
		surprisetalk$elm_bulma$Helpers$node,
		'div',
		_List_fromArray(
			[
				surprisetalk$elm_bulma$Bulma$Classes$columns,
				function () {
				if (centered) {
					return surprisetalk$elm_bulma$Bulma$Classes$isCentered;
				} else {
					return surprisetalk$elm_bulma$Bulma$Classes$none;
				}
			}(),
				function () {
				if (multiline) {
					return surprisetalk$elm_bulma$Bulma$Classes$isMultiline;
				} else {
					return surprisetalk$elm_bulma$Bulma$Classes$none;
				}
			}(),
				function () {
				if (!gap) {
					return surprisetalk$elm_bulma$Bulma$Classes$isGapless;
				} else {
					return surprisetalk$elm_bulma$Bulma$Classes$none;
				}
			}(),
				function () {
				switch (gap) {
					case 0:
						return surprisetalk$elm_bulma$Bulma$Classes$is0;
					case 1:
						return surprisetalk$elm_bulma$Bulma$Classes$is1;
					case 2:
						return surprisetalk$elm_bulma$Bulma$Classes$is2;
					case 3:
						return surprisetalk$elm_bulma$Bulma$Classes$none;
					case 4:
						return surprisetalk$elm_bulma$Bulma$Classes$is4;
					case 5:
						return surprisetalk$elm_bulma$Bulma$Classes$is5;
					case 6:
						return surprisetalk$elm_bulma$Bulma$Classes$is6;
					case 7:
						return surprisetalk$elm_bulma$Bulma$Classes$is7;
					default:
						return surprisetalk$elm_bulma$Bulma$Classes$is8;
				}
			}(),
				function () {
				switch (display) {
					case 0:
						return surprisetalk$elm_bulma$Bulma$Classes$isMobile;
					case 1:
						return surprisetalk$elm_bulma$Bulma$Classes$none;
					default:
						return surprisetalk$elm_bulma$Bulma$Classes$isDesktop;
				}
			}()
			]));
};
var surprisetalk$elm_bulma$Bulma$Columns$Gap3 = 3;
var surprisetalk$elm_bulma$Bulma$Columns$TabletAndBeyond = 1;
var surprisetalk$elm_bulma$Bulma$Columns$columnsModifiers = {am: false, ap: 1, as: 3, bj: false};
var surprisetalk$elm_bulma$Bulma$Elements$H4 = 3;
var author$project$Page$Upload$demoArticle = F2(
	function (aTitle, someHtmls) {
		return A3(
			surprisetalk$elm_bulma$Bulma$Columns$columns,
			surprisetalk$elm_bulma$Bulma$Columns$columnsModifiers,
			_List_Nil,
			_List_fromArray(
				[
					A3(
					surprisetalk$elm_bulma$Bulma$Columns$column,
					A2(
						author$project$Page$Upload$myColumnModifiers,
						0,
						elm$core$Maybe$Just(2)),
					_List_Nil,
					_List_fromArray(
						[
							A3(
							surprisetalk$elm_bulma$Bulma$Elements$title,
							3,
							_List_Nil,
							_List_fromArray(
								[
									A2(
									elm$html$Html$strong,
									_List_Nil,
									_List_fromArray(
										[
											elm$html$Html$text(aTitle)
										]))
								]))
						])),
					A3(
					surprisetalk$elm_bulma$Bulma$Columns$column,
					A2(
						author$project$Page$Upload$myColumnModifiers,
						0,
						elm$core$Maybe$Just(0)),
					_List_Nil,
					someHtmls)
				]));
	});
var surprisetalk$elm_bulma$Bulma$Classes$subtitle = elm$html$Html$Attributes$class('subtitle');
var surprisetalk$elm_bulma$Bulma$Elements$subtitle = function (size) {
	return A2(
		surprisetalk$elm_bulma$Helpers$node,
		function () {
			switch (size) {
				case 0:
					return 'h1';
				case 1:
					return 'h2';
				case 2:
					return 'h3';
				case 3:
					return 'h4';
				case 4:
					return 'h5';
				default:
					return 'h6';
			}
		}(),
		_List_fromArray(
			[
				surprisetalk$elm_bulma$Bulma$Classes$subtitle,
				function () {
				switch (size) {
					case 0:
						return surprisetalk$elm_bulma$Bulma$Classes$is1;
					case 1:
						return surprisetalk$elm_bulma$Bulma$Classes$is2;
					case 2:
						return surprisetalk$elm_bulma$Bulma$Classes$is3;
					case 3:
						return surprisetalk$elm_bulma$Bulma$Classes$is4;
					case 4:
						return surprisetalk$elm_bulma$Bulma$Classes$is5;
					default:
						return surprisetalk$elm_bulma$Bulma$Classes$is6;
				}
			}()
			]));
};
var surprisetalk$elm_bulma$Bulma$Modifiers$Width3 = 3;
var author$project$Page$Upload$demoSection = F3(
	function (aSubtitle, someAttrs, someHtmls) {
		return A3(
			surprisetalk$elm_bulma$Bulma$Columns$columns,
			surprisetalk$elm_bulma$Bulma$Columns$columnsModifiers,
			someAttrs,
			_List_fromArray(
				[
					A3(
					surprisetalk$elm_bulma$Bulma$Columns$column,
					A2(
						author$project$Page$Upload$myColumnModifiers,
						0,
						elm$core$Maybe$Just(3)),
					_List_Nil,
					_List_fromArray(
						[
							A3(
							surprisetalk$elm_bulma$Bulma$Elements$subtitle,
							3,
							_List_Nil,
							_List_fromArray(
								[
									elm$html$Html$text(aSubtitle)
								]))
						])),
					A3(
					surprisetalk$elm_bulma$Bulma$Columns$column,
					A2(
						author$project$Page$Upload$myColumnModifiers,
						0,
						elm$core$Maybe$Just(0)),
					_List_Nil,
					someHtmls)
				]));
	});
var elm$html$Html$br = _VirtualDom_node('br');
var elm$html$Html$small = _VirtualDom_node('small');
var surprisetalk$elm_bulma$Bulma$Elements$OneByOne = function (a) {
	return {$: 1, a: a};
};
var surprisetalk$elm_bulma$Bulma$Elements$X64 = 4;
var surprisetalk$elm_bulma$Bulma$Classes$image = elm$html$Html$Attributes$class('image');
var surprisetalk$elm_bulma$Bulma$Classes$is128x128 = elm$html$Html$Attributes$class('is-128x128');
var surprisetalk$elm_bulma$Bulma$Classes$is16by9 = elm$html$Html$Attributes$class('is-16by9');
var surprisetalk$elm_bulma$Bulma$Classes$is16x16 = elm$html$Html$Attributes$class('is-16x16');
var surprisetalk$elm_bulma$Bulma$Classes$is1by1 = elm$html$Html$Attributes$class('is-1by1');
var surprisetalk$elm_bulma$Bulma$Classes$is24x24 = elm$html$Html$Attributes$class('is-24x24');
var surprisetalk$elm_bulma$Bulma$Classes$is2by1 = elm$html$Html$Attributes$class('is-2by1');
var surprisetalk$elm_bulma$Bulma$Classes$is32x32 = elm$html$Html$Attributes$class('is-32x32');
var surprisetalk$elm_bulma$Bulma$Classes$is3by2 = elm$html$Html$Attributes$class('is-3by2');
var surprisetalk$elm_bulma$Bulma$Classes$is48x48 = elm$html$Html$Attributes$class('is-48x48');
var surprisetalk$elm_bulma$Bulma$Classes$is4by3 = elm$html$Html$Attributes$class('is-4by3');
var surprisetalk$elm_bulma$Bulma$Classes$is64x64 = elm$html$Html$Attributes$class('is-64x64');
var surprisetalk$elm_bulma$Bulma$Classes$is96x96 = elm$html$Html$Attributes$class('is-96x96');
var surprisetalk$elm_bulma$Bulma$Elements$image = function (shape) {
	return A2(
		surprisetalk$elm_bulma$Helpers$node,
		'figure',
		_List_fromArray(
			[
				surprisetalk$elm_bulma$Bulma$Classes$image,
				function () {
				switch (shape.$) {
					case 1:
						switch (shape.a) {
							case 7:
								var _n1 = shape.a;
								return surprisetalk$elm_bulma$Bulma$Classes$is1by1;
							case 0:
								var _n2 = shape.a;
								return surprisetalk$elm_bulma$Bulma$Classes$is16x16;
							case 1:
								var _n3 = shape.a;
								return surprisetalk$elm_bulma$Bulma$Classes$is24x24;
							case 2:
								var _n4 = shape.a;
								return surprisetalk$elm_bulma$Bulma$Classes$is32x32;
							case 3:
								var _n5 = shape.a;
								return surprisetalk$elm_bulma$Bulma$Classes$is48x48;
							case 4:
								var _n6 = shape.a;
								return surprisetalk$elm_bulma$Bulma$Classes$is64x64;
							case 5:
								var _n7 = shape.a;
								return surprisetalk$elm_bulma$Bulma$Classes$is96x96;
							default:
								var _n8 = shape.a;
								return surprisetalk$elm_bulma$Bulma$Classes$is128x128;
						}
					case 2:
						return surprisetalk$elm_bulma$Bulma$Classes$is4by3;
					case 3:
						return surprisetalk$elm_bulma$Bulma$Classes$is3by2;
					case 4:
						return surprisetalk$elm_bulma$Bulma$Classes$is16by9;
					case 5:
						return surprisetalk$elm_bulma$Bulma$Classes$is2by1;
					default:
						return surprisetalk$elm_bulma$Bulma$Classes$none;
				}
			}()
			]));
};
var surprisetalk$elm_bulma$Bulma$Classes$isHorizontal = elm$html$Html$Attributes$class('is-horizontal');
var surprisetalk$elm_bulma$Bulma$Classes$level = elm$html$Html$Attributes$class('level');
var surprisetalk$elm_bulma$Bulma$Layout$horizontalLevel = A2(
	surprisetalk$elm_bulma$Helpers$node,
	'nav',
	_List_fromArray(
		[surprisetalk$elm_bulma$Bulma$Classes$level, surprisetalk$elm_bulma$Bulma$Classes$isHorizontal]));
var surprisetalk$elm_bulma$Bulma$Classes$levelItem = elm$html$Html$Attributes$class('level-item');
var surprisetalk$elm_bulma$Bulma$Layout$levelItemLink = A2(
	surprisetalk$elm_bulma$Helpers$node,
	'a',
	_List_fromArray(
		[surprisetalk$elm_bulma$Bulma$Classes$levelItem]));
var surprisetalk$elm_bulma$Bulma$Classes$levelLeft = elm$html$Html$Attributes$class('level-left');
var surprisetalk$elm_bulma$Bulma$Layout$levelLeft = A2(
	surprisetalk$elm_bulma$Helpers$node,
	'div',
	_List_fromArray(
		[surprisetalk$elm_bulma$Bulma$Classes$levelLeft]));
var surprisetalk$elm_bulma$Bulma$Classes$media = elm$html$Html$Attributes$class('media');
var surprisetalk$elm_bulma$Bulma$Layout$media = A2(
	surprisetalk$elm_bulma$Helpers$node,
	'article',
	_List_fromArray(
		[surprisetalk$elm_bulma$Bulma$Classes$media]));
var surprisetalk$elm_bulma$Bulma$Classes$mediaContent = elm$html$Html$Attributes$class('media-content');
var surprisetalk$elm_bulma$Bulma$Layout$mediaContent = A2(
	surprisetalk$elm_bulma$Helpers$node,
	'div',
	_List_fromArray(
		[surprisetalk$elm_bulma$Bulma$Classes$mediaContent]));
var surprisetalk$elm_bulma$Bulma$Classes$mediaLeft = elm$html$Html$Attributes$class('media-left');
var surprisetalk$elm_bulma$Bulma$Layout$mediaLeft = A2(
	surprisetalk$elm_bulma$Helpers$node,
	'div',
	_List_fromArray(
		[surprisetalk$elm_bulma$Bulma$Classes$mediaLeft]));
var author$project$Page$Upload$exampleMediaObject = A2(
	surprisetalk$elm_bulma$Bulma$Layout$media,
	_List_Nil,
	_List_fromArray(
		[
			A2(
			surprisetalk$elm_bulma$Bulma$Layout$mediaLeft,
			_List_Nil,
			_List_fromArray(
				[
					A3(
					surprisetalk$elm_bulma$Bulma$Elements$image,
					surprisetalk$elm_bulma$Bulma$Elements$OneByOne(4),
					_List_Nil,
					_List_fromArray(
						[
							A2(
							elm$html$Html$img,
							_List_fromArray(
								[
									elm$html$Html$Attributes$src('https://bulma.io/images/placeholders/128x128.png')
								]),
							_List_Nil)
						]))
				])),
			A2(
			surprisetalk$elm_bulma$Bulma$Layout$mediaContent,
			_List_Nil,
			_List_fromArray(
				[
					A3(
					surprisetalk$elm_bulma$Bulma$Elements$content,
					1,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							elm$html$Html$p,
							_List_Nil,
							_List_fromArray(
								[
									A2(
									elm$html$Html$strong,
									_List_Nil,
									_List_fromArray(
										[
											elm$html$Html$text('John Smith')
										])),
									elm$html$Html$text(' '),
									A2(
									elm$html$Html$small,
									_List_Nil,
									_List_fromArray(
										[
											elm$html$Html$text('@johnsmith')
										])),
									elm$html$Html$text(' '),
									A2(
									elm$html$Html$small,
									_List_Nil,
									_List_fromArray(
										[
											elm$html$Html$text('31m')
										])),
									A2(elm$html$Html$br, _List_Nil, _List_Nil),
									elm$html$Html$text('Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aenean efficitur sit amet massa fringilla egestas. Nullam condimentum luctus turpis.')
								]))
						])),
					A2(
					surprisetalk$elm_bulma$Bulma$Layout$horizontalLevel,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							surprisetalk$elm_bulma$Bulma$Layout$levelLeft,
							_List_Nil,
							_List_fromArray(
								[
									A2(
									surprisetalk$elm_bulma$Bulma$Layout$levelItemLink,
									_List_Nil,
									_List_fromArray(
										[
											A3(
											surprisetalk$elm_bulma$Bulma$Elements$icon,
											0,
											_List_Nil,
											_List_fromArray(
												[
													A2(
													elm$html$Html$i,
													_List_fromArray(
														[
															elm$html$Html$Attributes$class('fa fa-reply')
														]),
													_List_Nil)
												]))
										])),
									A2(
									surprisetalk$elm_bulma$Bulma$Layout$levelItemLink,
									_List_Nil,
									_List_fromArray(
										[
											A3(
											surprisetalk$elm_bulma$Bulma$Elements$icon,
											0,
											_List_Nil,
											_List_fromArray(
												[
													A2(
													elm$html$Html$i,
													_List_fromArray(
														[
															elm$html$Html$Attributes$class('fa fa-retweet')
														]),
													_List_Nil)
												]))
										])),
									A2(
									surprisetalk$elm_bulma$Bulma$Layout$levelItemLink,
									_List_Nil,
									_List_fromArray(
										[
											A3(
											surprisetalk$elm_bulma$Bulma$Elements$icon,
											0,
											_List_Nil,
											_List_fromArray(
												[
													A2(
													elm$html$Html$i,
													_List_fromArray(
														[
															elm$html$Html$Attributes$class('fa fa-heart')
														]),
													_List_Nil)
												]))
										]))
								]))
						]))
				]))
		]));
var elm$html$Html$option = _VirtualDom_node('option');
var elm$virtual_dom$VirtualDom$attribute = F2(
	function (key, value) {
		return A2(
			_VirtualDom_attribute,
			_VirtualDom_noOnOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var elm$html$Html$Attributes$attribute = elm$virtual_dom$VirtualDom$attribute;
var elm$html$Html$Attributes$placeholder = elm$html$Html$Attributes$stringProperty('placeholder');
var surprisetalk$elm_bulma$Bulma$Components$Boxed = 1;
var surprisetalk$elm_bulma$Bulma$Classes$card = elm$html$Html$Attributes$class('card');
var surprisetalk$elm_bulma$Bulma$Components$card = A2(
	surprisetalk$elm_bulma$Helpers$node,
	'div',
	_List_fromArray(
		[surprisetalk$elm_bulma$Bulma$Classes$card]));
var surprisetalk$elm_bulma$Bulma$Classes$cardContent = elm$html$Html$Attributes$class('card-content');
var surprisetalk$elm_bulma$Bulma$Components$cardContent = A2(
	surprisetalk$elm_bulma$Helpers$node,
	'div',
	_List_fromArray(
		[surprisetalk$elm_bulma$Bulma$Classes$cardContent]));
var surprisetalk$elm_bulma$Bulma$Classes$cardFooter = elm$html$Html$Attributes$class('card-footer');
var surprisetalk$elm_bulma$Bulma$Components$cardFooter = A2(
	surprisetalk$elm_bulma$Helpers$node,
	'footer',
	_List_fromArray(
		[surprisetalk$elm_bulma$Bulma$Classes$cardFooter]));
var surprisetalk$elm_bulma$Bulma$Classes$cardFooterItem = elm$html$Html$Attributes$class('card-footer-item');
var surprisetalk$elm_bulma$Bulma$Components$cardFooterItem = A2(
	surprisetalk$elm_bulma$Helpers$node,
	'p',
	_List_fromArray(
		[surprisetalk$elm_bulma$Bulma$Classes$cardFooterItem]));
var surprisetalk$elm_bulma$Bulma$Classes$cardImage = elm$html$Html$Attributes$class('card-image');
var surprisetalk$elm_bulma$Bulma$Components$cardImage = A2(
	surprisetalk$elm_bulma$Helpers$node,
	'div',
	_List_fromArray(
		[surprisetalk$elm_bulma$Bulma$Classes$cardImage]));
var surprisetalk$elm_bulma$Bulma$Classes$dropdown = elm$html$Html$Attributes$class('dropdown');
var surprisetalk$elm_bulma$Bulma$Classes$isRight = elm$html$Html$Attributes$class('is-right');
var surprisetalk$elm_bulma$Bulma$Classes$isUp = elm$html$Html$Attributes$class('is-up');
var surprisetalk$elm_bulma$Bulma$Components$dropdown = F2(
	function (isActive, _n0) {
		var horizontalAlignment = _n0.aa;
		var verticalDirection = _n0.af;
		return A2(
			surprisetalk$elm_bulma$Helpers$node,
			'div',
			_List_fromArray(
				[
					surprisetalk$elm_bulma$Bulma$Classes$dropdown,
					function () {
					if (isActive) {
						return surprisetalk$elm_bulma$Bulma$Classes$isActive;
					} else {
						return surprisetalk$elm_bulma$Bulma$Classes$none;
					}
				}(),
					function () {
					if (horizontalAlignment === 2) {
						return surprisetalk$elm_bulma$Bulma$Classes$isRight;
					} else {
						return surprisetalk$elm_bulma$Bulma$Classes$none;
					}
				}(),
					function () {
					if (!verticalDirection) {
						return surprisetalk$elm_bulma$Bulma$Classes$isUp;
					} else {
						return surprisetalk$elm_bulma$Bulma$Classes$none;
					}
				}()
				]));
	});
var surprisetalk$elm_bulma$Bulma$Classes$dropdownDivider = elm$html$Html$Attributes$class('dropdown-divider');
var surprisetalk$elm_bulma$Bulma$Components$dropdownDivider = A2(
	surprisetalk$elm_bulma$Helpers$node,
	'hr',
	_List_fromArray(
		[surprisetalk$elm_bulma$Bulma$Classes$dropdownDivider]));
var surprisetalk$elm_bulma$Bulma$Classes$dropdownItem = elm$html$Html$Attributes$class('dropdown-item');
var surprisetalk$elm_bulma$Bulma$Components$dropdownItem = function (isActive) {
	return A2(
		surprisetalk$elm_bulma$Helpers$node,
		'div',
		_List_fromArray(
			[
				surprisetalk$elm_bulma$Bulma$Classes$dropdownItem,
				isActive ? surprisetalk$elm_bulma$Bulma$Classes$isActive : surprisetalk$elm_bulma$Bulma$Classes$none
			]));
};
var surprisetalk$elm_bulma$Bulma$Components$dropdownItemLink = function (isActive) {
	return A2(
		surprisetalk$elm_bulma$Helpers$node,
		'a',
		_List_fromArray(
			[
				surprisetalk$elm_bulma$Bulma$Classes$dropdownItem,
				isActive ? surprisetalk$elm_bulma$Bulma$Classes$isActive : surprisetalk$elm_bulma$Bulma$Classes$none
			]));
};
var surprisetalk$elm_bulma$Bulma$Components$dropdownMenu = F3(
	function (attrs, attrs_, items) {
		return A2(
			elm$html$Html$div,
			_Utils_ap(
				attrs,
				_List_fromArray(
					[
						elm$html$Html$Attributes$class('dropdown-menu'),
						A2(elm$html$Html$Attributes$attribute, 'role', 'menu')
					])),
			_List_fromArray(
				[
					A2(
					elm$html$Html$div,
					A2(
						elm$core$List$cons,
						elm$html$Html$Attributes$class('dropdown-content'),
						attrs_),
					items)
				]));
	});
var surprisetalk$elm_bulma$Bulma$Modifiers$Down = 1;
var surprisetalk$elm_bulma$Bulma$Components$dropdownModifiers = {aa: 0, af: 1};
var surprisetalk$elm_bulma$Bulma$Classes$dropdownTrigger = elm$html$Html$Attributes$class('dropdown-trigger');
var surprisetalk$elm_bulma$Bulma$Components$dropdownTrigger = A2(
	surprisetalk$elm_bulma$Helpers$node,
	'div',
	_List_fromArray(
		[surprisetalk$elm_bulma$Bulma$Classes$dropdownTrigger]));
var surprisetalk$elm_bulma$Bulma$Classes$menu = elm$html$Html$Attributes$class('menu');
var surprisetalk$elm_bulma$Bulma$Components$menu = A2(
	surprisetalk$elm_bulma$Helpers$node,
	'aside',
	_List_fromArray(
		[surprisetalk$elm_bulma$Bulma$Classes$menu]));
var surprisetalk$elm_bulma$Bulma$Classes$menuLabel = elm$html$Html$Attributes$class('menu-label');
var surprisetalk$elm_bulma$Bulma$Components$menuLabel = A2(
	surprisetalk$elm_bulma$Helpers$node,
	'p',
	_List_fromArray(
		[surprisetalk$elm_bulma$Bulma$Classes$menuLabel]));
var surprisetalk$elm_bulma$Bulma$Classes$menuList = elm$html$Html$Attributes$class('menu-list');
var surprisetalk$elm_bulma$Bulma$Components$menuList = A2(
	surprisetalk$elm_bulma$Helpers$node,
	'ul',
	_List_fromArray(
		[surprisetalk$elm_bulma$Bulma$Classes$menuList]));
var elm$html$Html$li = _VirtualDom_node('li');
var surprisetalk$elm_bulma$Bulma$Components$menuListItem = elm$html$Html$li;
var surprisetalk$elm_bulma$Bulma$Components$menuListItemLink = function (active) {
	return A2(
		elm$core$Basics$composeL,
		elm$html$Html$a,
		elm$core$List$cons(
			elm$html$Html$Attributes$class(
				active ? 'is-active' : '')));
};
var surprisetalk$elm_bulma$Bulma$Classes$message = elm$html$Html$Attributes$class('message');
var surprisetalk$elm_bulma$Bulma$Components$message = function (_n0) {
	var color = _n0.b_;
	var size = _n0.q;
	return A2(
		surprisetalk$elm_bulma$Helpers$node,
		'article',
		_List_fromArray(
			[
				surprisetalk$elm_bulma$Bulma$Classes$message,
				function () {
				switch (color) {
					case 0:
						return surprisetalk$elm_bulma$Bulma$Classes$none;
					case 1:
						return surprisetalk$elm_bulma$Bulma$Classes$isWhite;
					case 2:
						return surprisetalk$elm_bulma$Bulma$Classes$isLight;
					case 3:
						return surprisetalk$elm_bulma$Bulma$Classes$isDark;
					case 4:
						return surprisetalk$elm_bulma$Bulma$Classes$isBlack;
					case 5:
						return surprisetalk$elm_bulma$Bulma$Classes$isPrimary;
					case 6:
						return surprisetalk$elm_bulma$Bulma$Classes$isLink;
					case 7:
						return surprisetalk$elm_bulma$Bulma$Classes$isInfo;
					case 8:
						return surprisetalk$elm_bulma$Bulma$Classes$isSuccess;
					case 9:
						return surprisetalk$elm_bulma$Bulma$Classes$isWarning;
					default:
						return surprisetalk$elm_bulma$Bulma$Classes$isDanger;
				}
			}(),
				function () {
				switch (size) {
					case 0:
						return surprisetalk$elm_bulma$Bulma$Classes$isSmall;
					case 1:
						return surprisetalk$elm_bulma$Bulma$Classes$none;
					case 2:
						return surprisetalk$elm_bulma$Bulma$Classes$isMedium;
					default:
						return surprisetalk$elm_bulma$Bulma$Classes$isLarge;
				}
			}()
			]));
};
var surprisetalk$elm_bulma$Bulma$Classes$messageBody = elm$html$Html$Attributes$class('message-body');
var surprisetalk$elm_bulma$Bulma$Components$messageBody = A2(
	surprisetalk$elm_bulma$Helpers$node,
	'div',
	_List_fromArray(
		[surprisetalk$elm_bulma$Bulma$Classes$messageBody]));
var surprisetalk$elm_bulma$Bulma$Classes$messageHeader = elm$html$Html$Attributes$class('message-header');
var elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 0, a: a};
};
var elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			elm$virtual_dom$VirtualDom$on,
			event,
			elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var elm$html$Html$Events$onClick = function (msg) {
	return A2(
		elm$html$Html$Events$on,
		'click',
		elm$json$Json$Decode$succeed(msg));
};
var surprisetalk$elm_bulma$Bulma$Classes$delete = elm$html$Html$Attributes$class('delete');
var surprisetalk$elm_bulma$Bulma$Elements$delete = A2(
	surprisetalk$elm_bulma$Helpers$node,
	'a',
	_List_fromArray(
		[surprisetalk$elm_bulma$Bulma$Classes$delete]));
var surprisetalk$elm_bulma$Bulma$Elements$easyDelete = F2(
	function (attrs, msg) {
		return A2(
			surprisetalk$elm_bulma$Bulma$Elements$delete,
			A2(
				elm$core$List$cons,
				elm$html$Html$Events$onClick(msg),
				attrs),
			_List_Nil);
	});
var surprisetalk$elm_bulma$Helpers$flip = F3(
	function (f, a, b) {
		return A2(f, b, a);
	});
var surprisetalk$elm_bulma$Bulma$Components$messageHeaderWithDelete = F2(
	function (attrs, msg) {
		return A2(
			elm$core$Basics$composeL,
			A3(
				surprisetalk$elm_bulma$Helpers$node,
				'div',
				_List_fromArray(
					[surprisetalk$elm_bulma$Bulma$Classes$messageHeader]),
				attrs),
			A2(
				surprisetalk$elm_bulma$Helpers$flip,
				elm$core$Basics$append,
				_List_fromArray(
					[
						A2(surprisetalk$elm_bulma$Bulma$Elements$easyDelete, _List_Nil, msg)
					])));
	});
var surprisetalk$elm_bulma$Bulma$Components$messageModifiers = {b_: 0, q: 1};
var surprisetalk$elm_bulma$Bulma$Classes$pagination = elm$html$Html$Attributes$class('pagination');
var surprisetalk$elm_bulma$Bulma$Components$pagination = function (alignment) {
	return A2(
		surprisetalk$elm_bulma$Helpers$node,
		'div',
		_List_fromArray(
			[
				surprisetalk$elm_bulma$Bulma$Classes$pagination,
				function () {
				switch (alignment) {
					case 0:
						return surprisetalk$elm_bulma$Bulma$Classes$none;
					case 1:
						return surprisetalk$elm_bulma$Bulma$Classes$isCentered;
					default:
						return surprisetalk$elm_bulma$Bulma$Classes$isRight;
				}
			}()
			]));
};
var surprisetalk$elm_bulma$Bulma$Classes$paginationEllipsis = elm$html$Html$Attributes$class('pagination-ellipsis');
var surprisetalk$elm_bulma$Bulma$Components$paginationEllipsis = A2(
	surprisetalk$elm_bulma$Helpers$node,
	'span',
	_List_fromArray(
		[surprisetalk$elm_bulma$Bulma$Classes$paginationEllipsis]));
var surprisetalk$elm_bulma$Bulma$Classes$isCurrent = elm$html$Html$Attributes$class('is-current');
var surprisetalk$elm_bulma$Bulma$Classes$paginationLink = elm$html$Html$Attributes$class('pagination-link');
var surprisetalk$elm_bulma$Bulma$Components$paginationLink = function (current) {
	return A2(
		surprisetalk$elm_bulma$Helpers$node,
		'a',
		_List_fromArray(
			[
				surprisetalk$elm_bulma$Bulma$Classes$paginationLink,
				function () {
				if (current) {
					return surprisetalk$elm_bulma$Bulma$Classes$isCurrent;
				} else {
					return surprisetalk$elm_bulma$Bulma$Classes$none;
				}
			}()
			]));
};
var surprisetalk$elm_bulma$Bulma$Classes$paginationList = elm$html$Html$Attributes$class('pagination-list');
var surprisetalk$elm_bulma$Helpers$ls = function (x) {
	return _List_fromArray(
		[x]);
};
var surprisetalk$elm_bulma$Bulma$Components$paginationList = function (attrs) {
	return A2(
		elm$core$Basics$composeR,
		elm$core$List$map(
			A2(
				elm$core$Basics$composeR,
				surprisetalk$elm_bulma$Helpers$ls,
				elm$html$Html$li(_List_Nil))),
		A3(
			surprisetalk$elm_bulma$Helpers$node,
			'ul',
			_List_fromArray(
				[surprisetalk$elm_bulma$Bulma$Classes$paginationList]),
			attrs));
};
var surprisetalk$elm_bulma$Bulma$Classes$paginationNext = elm$html$Html$Attributes$class('pagination-next');
var surprisetalk$elm_bulma$Bulma$Components$paginationNext = A2(
	surprisetalk$elm_bulma$Helpers$node,
	'a',
	_List_fromArray(
		[surprisetalk$elm_bulma$Bulma$Classes$paginationNext]));
var surprisetalk$elm_bulma$Bulma$Classes$paginationPrevious = elm$html$Html$Attributes$class('pagination-previous');
var surprisetalk$elm_bulma$Bulma$Components$paginationPrev = A2(
	surprisetalk$elm_bulma$Helpers$node,
	'a',
	_List_fromArray(
		[surprisetalk$elm_bulma$Bulma$Classes$paginationPrevious]));
var surprisetalk$elm_bulma$Bulma$Classes$panel = elm$html$Html$Attributes$class('panel');
var surprisetalk$elm_bulma$Bulma$Components$panel = A2(
	surprisetalk$elm_bulma$Helpers$node,
	'div',
	_List_fromArray(
		[surprisetalk$elm_bulma$Bulma$Classes$panel]));
var surprisetalk$elm_bulma$Bulma$Classes$panelBlock = elm$html$Html$Attributes$class('panel-block');
var surprisetalk$elm_bulma$Bulma$Components$panelBlock = function (active) {
	return A2(
		surprisetalk$elm_bulma$Helpers$node,
		'div',
		_List_fromArray(
			[
				surprisetalk$elm_bulma$Bulma$Classes$panelBlock,
				function () {
				if (active) {
					return surprisetalk$elm_bulma$Bulma$Classes$isActive;
				} else {
					return surprisetalk$elm_bulma$Bulma$Classes$none;
				}
			}()
			]));
};
var elm$html$Html$input = _VirtualDom_node('input');
var elm$html$Html$Attributes$type_ = elm$html$Html$Attributes$stringProperty('type');
var surprisetalk$elm_bulma$Bulma$Components$panelCheckbox = F4(
	function (active, attrs, attrs_, htmls) {
		return A2(
			elm$html$Html$a,
			A2(
				elm$core$List$cons,
				function () {
					if (active) {
						return elm$html$Html$Attributes$class('panel-block is-active');
					} else {
						return elm$html$Html$Attributes$class('panel-block ');
					}
				}(),
				attrs),
			A2(
				elm$core$List$cons,
				A2(
					elm$html$Html$input,
					A2(
						elm$core$List$cons,
						elm$html$Html$Attributes$type_('checkbox'),
						attrs_),
					_List_Nil),
				htmls));
	});
var surprisetalk$elm_bulma$Bulma$Classes$panelHeading = elm$html$Html$Attributes$class('panel-heading');
var surprisetalk$elm_bulma$Bulma$Components$panelHeading = A2(
	surprisetalk$elm_bulma$Helpers$node,
	'p',
	_List_fromArray(
		[surprisetalk$elm_bulma$Bulma$Classes$panelHeading]));
var surprisetalk$elm_bulma$Bulma$Components$panelLinkWithIcon = F5(
	function (active, attrs, attrs_, iconBodies, htmls) {
		return A2(
			elm$html$Html$a,
			A2(
				elm$core$List$cons,
				function () {
					if (active) {
						return elm$html$Html$Attributes$class('panel-block is-active');
					} else {
						return elm$html$Html$Attributes$class('panel-block ');
					}
				}(),
				attrs),
			A2(
				elm$core$List$cons,
				A2(
					elm$html$Html$span,
					A2(
						elm$core$List$cons,
						elm$html$Html$Attributes$class('panel-icon'),
						attrs_),
					iconBodies),
				htmls));
	});
var surprisetalk$elm_bulma$Bulma$Components$panelTab = function (active) {
	return A2(
		surprisetalk$elm_bulma$Helpers$node,
		'a',
		_List_fromArray(
			[
				function () {
				if (active) {
					return surprisetalk$elm_bulma$Bulma$Classes$isActive;
				} else {
					return surprisetalk$elm_bulma$Bulma$Classes$none;
				}
			}()
			]));
};
var surprisetalk$elm_bulma$Bulma$Classes$panelTabs = elm$html$Html$Attributes$class('panel-tabs');
var surprisetalk$elm_bulma$Bulma$Components$panelTabs = A2(
	surprisetalk$elm_bulma$Helpers$node,
	'p',
	_List_fromArray(
		[surprisetalk$elm_bulma$Bulma$Classes$panelTabs]));
var surprisetalk$elm_bulma$Bulma$Components$tab = F4(
	function (active, attrs, attrs_, htmls) {
		return A2(
			elm$html$Html$li,
			A2(
				elm$core$List$cons,
				function () {
					if (active) {
						return surprisetalk$elm_bulma$Bulma$Classes$isActive;
					} else {
						return surprisetalk$elm_bulma$Bulma$Classes$none;
					}
				}(),
				attrs),
			_List_fromArray(
				[
					A2(elm$html$Html$a, attrs_, htmls)
				]));
	});
var elm$html$Html$ul = _VirtualDom_node('ul');
var surprisetalk$elm_bulma$Bulma$Classes$isBoxed = elm$html$Html$Attributes$class('is-boxed');
var surprisetalk$elm_bulma$Bulma$Classes$isToggle = elm$html$Html$Attributes$class('is-toggle');
var surprisetalk$elm_bulma$Bulma$Classes$isToggleRounded = elm$html$Html$Attributes$class('is-toggle-rounded');
var surprisetalk$elm_bulma$Bulma$Classes$tabs = elm$html$Html$Attributes$class('tabs');
var surprisetalk$elm_bulma$Bulma$Components$tabs = F3(
	function (_n0, attrs, attrs_) {
		var style = _n0.cn;
		var alignment = _n0.z;
		var size = _n0.q;
		return A2(
			elm$core$Basics$composeL,
			A2(
				elm$core$Basics$composeL,
				A3(
					surprisetalk$elm_bulma$Helpers$node,
					'div',
					_List_fromArray(
						[
							surprisetalk$elm_bulma$Bulma$Classes$tabs,
							function () {
							switch (style) {
								case 2:
									return surprisetalk$elm_bulma$Bulma$Classes$isToggle;
								case 3:
									return surprisetalk$elm_bulma$Bulma$Classes$isToggle;
								default:
									return surprisetalk$elm_bulma$Bulma$Classes$none;
							}
						}(),
							function () {
							if (style === 1) {
								return surprisetalk$elm_bulma$Bulma$Classes$isBoxed;
							} else {
								return surprisetalk$elm_bulma$Bulma$Classes$none;
							}
						}(),
							function () {
							if (style === 3) {
								return surprisetalk$elm_bulma$Bulma$Classes$isToggleRounded;
							} else {
								return surprisetalk$elm_bulma$Bulma$Classes$none;
							}
						}(),
							function () {
							switch (alignment) {
								case 0:
									return surprisetalk$elm_bulma$Bulma$Classes$none;
								case 1:
									return surprisetalk$elm_bulma$Bulma$Classes$isCentered;
								default:
									return surprisetalk$elm_bulma$Bulma$Classes$isRight;
							}
						}(),
							function () {
							switch (size) {
								case 0:
									return surprisetalk$elm_bulma$Bulma$Classes$isSmall;
								case 1:
									return surprisetalk$elm_bulma$Bulma$Classes$none;
								case 2:
									return surprisetalk$elm_bulma$Bulma$Classes$isMedium;
								default:
									return surprisetalk$elm_bulma$Bulma$Classes$isLarge;
							}
						}()
						]),
					attrs),
				surprisetalk$elm_bulma$Helpers$ls),
			elm$html$Html$ul(attrs_));
	});
var surprisetalk$elm_bulma$Bulma$Components$Minimal = 0;
var surprisetalk$elm_bulma$Bulma$Components$tabsModifiers = {z: 0, q: 1, cn: 0};
var surprisetalk$elm_bulma$Bulma$Elements$FourByThree = {$: 2};
var surprisetalk$elm_bulma$Bulma$Elements$H3 = 2;
var surprisetalk$elm_bulma$Bulma$Elements$X48 = 3;
var surprisetalk$elm_bulma$Bulma$Classes$box = elm$html$Html$Attributes$class('box');
var surprisetalk$elm_bulma$Bulma$Elements$box = A2(
	surprisetalk$elm_bulma$Helpers$node,
	'div',
	_List_fromArray(
		[surprisetalk$elm_bulma$Bulma$Classes$box]));
var surprisetalk$elm_bulma$Bulma$Classes$buttons = elm$html$Html$Attributes$class('buttons');
var surprisetalk$elm_bulma$Bulma$Elements$buttons = function (alignment) {
	return A2(
		surprisetalk$elm_bulma$Helpers$node,
		'div',
		_List_fromArray(
			[
				surprisetalk$elm_bulma$Bulma$Classes$buttons,
				function () {
				switch (alignment) {
					case 0:
						return surprisetalk$elm_bulma$Bulma$Classes$none;
					case 1:
						return surprisetalk$elm_bulma$Bulma$Classes$isCentered;
					default:
						return surprisetalk$elm_bulma$Bulma$Classes$isRight;
				}
			}()
			]));
};
var elm$core$Basics$round = _Basics_round;
var elm$html$Html$Attributes$max = elm$html$Html$Attributes$stringProperty('max');
var elm$html$Html$Attributes$value = elm$html$Html$Attributes$stringProperty('value');
var surprisetalk$elm_bulma$Bulma$Classes$progress = elm$html$Html$Attributes$class('progress');
var surprisetalk$elm_bulma$Bulma$Elements$progress = function (_n0) {
	var size = _n0.q;
	var color = _n0.b_;
	return A2(
		surprisetalk$elm_bulma$Helpers$node,
		'progress',
		_List_fromArray(
			[
				surprisetalk$elm_bulma$Bulma$Classes$progress,
				function () {
				switch (size) {
					case 0:
						return surprisetalk$elm_bulma$Bulma$Classes$isSmall;
					case 1:
						return surprisetalk$elm_bulma$Bulma$Classes$none;
					case 2:
						return surprisetalk$elm_bulma$Bulma$Classes$isMedium;
					default:
						return surprisetalk$elm_bulma$Bulma$Classes$isLarge;
				}
			}(),
				function () {
				switch (color) {
					case 0:
						return surprisetalk$elm_bulma$Bulma$Classes$none;
					case 1:
						return surprisetalk$elm_bulma$Bulma$Classes$isWhite;
					case 2:
						return surprisetalk$elm_bulma$Bulma$Classes$isLight;
					case 3:
						return surprisetalk$elm_bulma$Bulma$Classes$isDark;
					case 4:
						return surprisetalk$elm_bulma$Bulma$Classes$isBlack;
					case 5:
						return surprisetalk$elm_bulma$Bulma$Classes$isPrimary;
					case 7:
						return surprisetalk$elm_bulma$Bulma$Classes$isInfo;
					case 8:
						return surprisetalk$elm_bulma$Bulma$Classes$isSuccess;
					case 9:
						return surprisetalk$elm_bulma$Bulma$Classes$isWarning;
					case 10:
						return surprisetalk$elm_bulma$Bulma$Classes$isDanger;
					default:
						return surprisetalk$elm_bulma$Bulma$Classes$isLink;
				}
			}()
			]));
};
var surprisetalk$elm_bulma$Bulma$Elements$easyProgress = F3(
	function (mods, attrs, val) {
		return A3(
			surprisetalk$elm_bulma$Bulma$Elements$progress,
			mods,
			A2(
				elm$core$List$cons,
				elm$html$Html$Attributes$value(
					elm$core$String$fromInt(
						elm$core$Basics$round(val * 100))),
				A2(
					elm$core$List$cons,
					elm$html$Html$Attributes$max('100'),
					attrs)),
			_List_Nil);
	});
var surprisetalk$elm_bulma$Bulma$Classes$isNormal = elm$html$Html$Attributes$class('is-normal');
var surprisetalk$elm_bulma$Bulma$Elements$easyTitleWithSubtitle = F4(
	function (spacing, size, title_, subt) {
		return _List_fromArray(
			[
				A4(
				surprisetalk$elm_bulma$Helpers$node,
				'p',
				_List_fromArray(
					[
						surprisetalk$elm_bulma$Bulma$Classes$title,
						function () {
						if (spacing) {
							return surprisetalk$elm_bulma$Bulma$Classes$isNormal;
						} else {
							return surprisetalk$elm_bulma$Bulma$Classes$none;
						}
					}(),
						function () {
						switch (size) {
							case 0:
								return surprisetalk$elm_bulma$Bulma$Classes$is1;
							case 1:
								return surprisetalk$elm_bulma$Bulma$Classes$is2;
							case 2:
								return surprisetalk$elm_bulma$Bulma$Classes$is3;
							case 3:
								return surprisetalk$elm_bulma$Bulma$Classes$is4;
							case 4:
								return surprisetalk$elm_bulma$Bulma$Classes$is5;
							default:
								return surprisetalk$elm_bulma$Bulma$Classes$is6;
						}
					}()
					]),
				_List_Nil,
				title_),
				A4(
				surprisetalk$elm_bulma$Helpers$node,
				'p',
				_List_fromArray(
					[
						surprisetalk$elm_bulma$Bulma$Classes$subtitle,
						function () {
						switch (size) {
							case 0:
								return surprisetalk$elm_bulma$Bulma$Classes$is3;
							case 1:
								return surprisetalk$elm_bulma$Bulma$Classes$is4;
							case 2:
								return surprisetalk$elm_bulma$Bulma$Classes$is5;
							default:
								return surprisetalk$elm_bulma$Bulma$Classes$is6;
						}
					}()
					]),
				_List_Nil,
				subt)
			]);
	});
var surprisetalk$elm_bulma$Bulma$Elements$notificationWithDelete = F3(
	function (color, attrs, msg) {
		return A2(
			elm$core$Basics$composeL,
			A2(surprisetalk$elm_bulma$Bulma$Elements$notification, color, attrs),
			elm$core$List$cons(
				A2(surprisetalk$elm_bulma$Bulma$Elements$easyDelete, _List_Nil, msg)));
	});
var surprisetalk$elm_bulma$Bulma$Elements$progressModifiers = {b_: 0, q: 1};
var surprisetalk$elm_bulma$Bulma$Elements$tags = A2(
	surprisetalk$elm_bulma$Helpers$node,
	'div',
	_List_fromArray(
		[surprisetalk$elm_bulma$Bulma$Classes$tags]));
var surprisetalk$elm_bulma$Bulma$Form$controlButton = F3(
	function (mods, attrs, attrs_) {
		return A2(
			elm$core$Basics$composeL,
			A2(
				elm$core$Basics$composeL,
				A2(surprisetalk$elm_bulma$Bulma$Form$control, surprisetalk$elm_bulma$Bulma$Form$controlModifiers, attrs),
				surprisetalk$elm_bulma$Helpers$ls),
			A2(surprisetalk$elm_bulma$Bulma$Elements$button, mods, attrs_));
	});
var surprisetalk$elm_bulma$Bulma$Classes$checkbox = elm$html$Html$Attributes$class('checkbox');
var surprisetalk$elm_bulma$Bulma$Form$controlCheckBox = F5(
	function (disabled, attrs, attrs_, attrs__, htmls) {
		return A3(
			surprisetalk$elm_bulma$Bulma$Form$control,
			surprisetalk$elm_bulma$Bulma$Form$controlModifiers,
			attrs,
			surprisetalk$elm_bulma$Helpers$ls(
				A4(
					surprisetalk$elm_bulma$Helpers$node,
					'label',
					_List_fromArray(
						[
							elm$html$Html$Attributes$disabled(disabled),
							surprisetalk$elm_bulma$Bulma$Classes$checkbox
						]),
					attrs_,
					A2(
						elm$core$List$cons,
						A4(
							surprisetalk$elm_bulma$Helpers$node,
							'input',
							_List_fromArray(
								[
									elm$html$Html$Attributes$disabled(disabled),
									elm$html$Html$Attributes$type_('checkbox')
								]),
							attrs__,
							_List_Nil),
						A2(
							elm$core$List$cons,
							elm$html$Html$text(' '),
							htmls)))));
	});
var surprisetalk$elm_bulma$Bulma$Classes$help = elm$html$Html$Attributes$class('help');
var surprisetalk$elm_bulma$Bulma$Form$help = function (color) {
	return A2(
		surprisetalk$elm_bulma$Helpers$node,
		'p',
		_List_fromArray(
			[
				surprisetalk$elm_bulma$Bulma$Classes$help,
				function () {
				switch (color) {
					case 0:
						return surprisetalk$elm_bulma$Bulma$Classes$none;
					case 1:
						return surprisetalk$elm_bulma$Bulma$Classes$isWhite;
					case 2:
						return surprisetalk$elm_bulma$Bulma$Classes$isLight;
					case 3:
						return surprisetalk$elm_bulma$Bulma$Classes$isDark;
					case 4:
						return surprisetalk$elm_bulma$Bulma$Classes$isBlack;
					case 5:
						return surprisetalk$elm_bulma$Bulma$Classes$isPrimary;
					case 6:
						return surprisetalk$elm_bulma$Bulma$Classes$isLink;
					case 7:
						return surprisetalk$elm_bulma$Bulma$Classes$isInfo;
					case 8:
						return surprisetalk$elm_bulma$Bulma$Classes$isSuccess;
					case 9:
						return surprisetalk$elm_bulma$Bulma$Classes$isWarning;
					default:
						return surprisetalk$elm_bulma$Bulma$Classes$isDanger;
				}
			}()
			]));
};
var surprisetalk$elm_bulma$Bulma$Form$controlHelp = surprisetalk$elm_bulma$Bulma$Form$help;
var elm$html$Html$Attributes$readonly = elm$html$Html$Attributes$boolProperty('readOnly');
var surprisetalk$elm_bulma$Bulma$Classes$input = elm$html$Html$Attributes$class('input');
var surprisetalk$elm_bulma$Bulma$Form$controlInput = F3(
	function (mods, attrs, attrs_) {
		var size = mods.q;
		var state = mods.bC;
		var color = mods.b_;
		var expanded = mods.h;
		var rounded = mods.aF;
		var readonly = mods.F;
		var disabled = mods.ao;
		var iconLeft = mods.au;
		var iconRight = mods.av;
		var controlMods = {
			h: expanded,
			au: iconLeft,
			av: iconRight,
			C: function () {
				if (state === 4) {
					return elm$core$Maybe$Just(size);
				} else {
					return elm$core$Maybe$Nothing;
				}
			}()
		};
		return A2(
			elm$core$Basics$composeL,
			A2(
				elm$core$Basics$composeL,
				A2(surprisetalk$elm_bulma$Bulma$Form$control, controlMods, attrs),
				surprisetalk$elm_bulma$Helpers$ls),
			A3(
				surprisetalk$elm_bulma$Helpers$node,
				'input',
				_List_fromArray(
					[
						surprisetalk$elm_bulma$Bulma$Classes$input,
						elm$html$Html$Attributes$disabled(disabled),
						function () {
						if (readonly) {
							return elm$html$Html$Attributes$readonly(readonly);
						} else {
							return surprisetalk$elm_bulma$Bulma$Classes$none;
						}
					}(),
						function () {
						switch (size) {
							case 0:
								return surprisetalk$elm_bulma$Bulma$Classes$isSmall;
							case 1:
								return surprisetalk$elm_bulma$Bulma$Classes$none;
							case 2:
								return surprisetalk$elm_bulma$Bulma$Classes$isMedium;
							default:
								return surprisetalk$elm_bulma$Bulma$Classes$isLarge;
						}
					}(),
						function () {
						switch (state) {
							case 1:
								return surprisetalk$elm_bulma$Bulma$Classes$isHovered;
							case 2:
								return surprisetalk$elm_bulma$Bulma$Classes$isFocused;
							case 3:
								return surprisetalk$elm_bulma$Bulma$Classes$isActive;
							case 4:
								return surprisetalk$elm_bulma$Bulma$Classes$isLoading;
							default:
								return surprisetalk$elm_bulma$Bulma$Classes$none;
						}
					}(),
						function () {
						switch (color) {
							case 0:
								return surprisetalk$elm_bulma$Bulma$Classes$none;
							case 1:
								return surprisetalk$elm_bulma$Bulma$Classes$isWhite;
							case 2:
								return surprisetalk$elm_bulma$Bulma$Classes$isLight;
							case 3:
								return surprisetalk$elm_bulma$Bulma$Classes$isDark;
							case 4:
								return surprisetalk$elm_bulma$Bulma$Classes$isBlack;
							case 5:
								return surprisetalk$elm_bulma$Bulma$Classes$isPrimary;
							case 7:
								return surprisetalk$elm_bulma$Bulma$Classes$isInfo;
							case 8:
								return surprisetalk$elm_bulma$Bulma$Classes$isSuccess;
							case 9:
								return surprisetalk$elm_bulma$Bulma$Classes$isWarning;
							case 10:
								return surprisetalk$elm_bulma$Bulma$Classes$isDanger;
							default:
								return surprisetalk$elm_bulma$Bulma$Classes$isLink;
						}
					}(),
						function () {
						if (!rounded) {
							return surprisetalk$elm_bulma$Bulma$Classes$none;
						} else {
							return surprisetalk$elm_bulma$Bulma$Classes$isRounded;
						}
					}()
					]),
				attrs_));
	});
var surprisetalk$elm_bulma$Bulma$Form$controlInputModifiers = {b_: 0, ao: false, h: false, au: elm$core$Maybe$Nothing, av: elm$core$Maybe$Nothing, F: false, aF: false, q: 1, bC: 0};
var surprisetalk$elm_bulma$Bulma$Classes$label = elm$html$Html$Attributes$class('label');
var surprisetalk$elm_bulma$Bulma$Form$label = A2(
	surprisetalk$elm_bulma$Helpers$node,
	'label',
	_List_fromArray(
		[surprisetalk$elm_bulma$Bulma$Classes$label]));
var surprisetalk$elm_bulma$Bulma$Form$controlLabel = surprisetalk$elm_bulma$Bulma$Form$label;
var surprisetalk$elm_bulma$Bulma$Form$controlRadio = surprisetalk$elm_bulma$Bulma$Form$control(surprisetalk$elm_bulma$Bulma$Form$controlModifiers);
var elm$html$Html$Attributes$checked = elm$html$Html$Attributes$boolProperty('checked');
var elm$html$Html$Attributes$name = elm$html$Html$Attributes$stringProperty('name');
var surprisetalk$elm_bulma$Bulma$Classes$radio = elm$html$Html$Attributes$class('radio');
var surprisetalk$elm_bulma$Bulma$Form$controlRadioButton = F6(
	function (disabled, checked, name, attrs, attrs_, htmls) {
		var labelAttrs = _List_fromArray(
			[
				surprisetalk$elm_bulma$Bulma$Classes$radio,
				elm$html$Html$Attributes$disabled(disabled)
			]);
		var inputAttrs = _List_fromArray(
			[
				elm$html$Html$Attributes$name(name),
				elm$html$Html$Attributes$type_('radio'),
				elm$html$Html$Attributes$disabled(disabled),
				elm$html$Html$Attributes$checked(checked)
			]);
		return A4(
			surprisetalk$elm_bulma$Helpers$node,
			'label',
			labelAttrs,
			attrs,
			A2(
				elm$core$List$cons,
				A4(surprisetalk$elm_bulma$Helpers$node, 'input', inputAttrs, attrs_, _List_Nil),
				A2(
					elm$core$List$cons,
					elm$html$Html$text(' '),
					htmls)));
	});
var surprisetalk$elm_bulma$Bulma$Classes$isFullWidth = elm$html$Html$Attributes$class('is-fullwidth');
var surprisetalk$elm_bulma$Bulma$Classes$select = elm$html$Html$Attributes$class('select');
var surprisetalk$elm_bulma$Bulma$Form$controlSelect = F3(
	function (mods, attrs, attrs_) {
		var size = mods.q;
		var state = mods.bC;
		var color = mods.b_;
		var expanded = mods.h;
		var iconLeft = mods.au;
		var controlMods = {
			h: expanded,
			au: iconLeft,
			av: elm$core$Maybe$Nothing,
			C: function () {
				if (state === 4) {
					return elm$core$Maybe$Just(size);
				} else {
					return elm$core$Maybe$Nothing;
				}
			}()
		};
		return A2(
			elm$core$Basics$composeL,
			A2(
				elm$core$Basics$composeL,
				A2(
					elm$core$Basics$composeL,
					A2(
						elm$core$Basics$composeL,
						A2(surprisetalk$elm_bulma$Bulma$Form$control, controlMods, attrs),
						surprisetalk$elm_bulma$Helpers$ls),
					A3(
						surprisetalk$elm_bulma$Helpers$node,
						'span',
						_List_fromArray(
							[surprisetalk$elm_bulma$Bulma$Classes$select]),
						_List_Nil)),
				surprisetalk$elm_bulma$Helpers$ls),
			A3(
				surprisetalk$elm_bulma$Helpers$node,
				'select',
				_List_fromArray(
					[
						function () {
						switch (size) {
							case 0:
								return surprisetalk$elm_bulma$Bulma$Classes$isSmall;
							case 1:
								return surprisetalk$elm_bulma$Bulma$Classes$none;
							case 2:
								return surprisetalk$elm_bulma$Bulma$Classes$isMedium;
							default:
								return surprisetalk$elm_bulma$Bulma$Classes$isLarge;
						}
					}(),
						function () {
						switch (state) {
							case 1:
								return surprisetalk$elm_bulma$Bulma$Classes$isHovered;
							case 2:
								return surprisetalk$elm_bulma$Bulma$Classes$isFocused;
							case 3:
								return surprisetalk$elm_bulma$Bulma$Classes$isActive;
							case 4:
								return surprisetalk$elm_bulma$Bulma$Classes$isLoading;
							default:
								return surprisetalk$elm_bulma$Bulma$Classes$none;
						}
					}(),
						function () {
						switch (color) {
							case 0:
								return surprisetalk$elm_bulma$Bulma$Classes$none;
							case 1:
								return surprisetalk$elm_bulma$Bulma$Classes$isWhite;
							case 2:
								return surprisetalk$elm_bulma$Bulma$Classes$isLight;
							case 3:
								return surprisetalk$elm_bulma$Bulma$Classes$isDark;
							case 4:
								return surprisetalk$elm_bulma$Bulma$Classes$isBlack;
							case 5:
								return surprisetalk$elm_bulma$Bulma$Classes$isPrimary;
							case 7:
								return surprisetalk$elm_bulma$Bulma$Classes$isInfo;
							case 8:
								return surprisetalk$elm_bulma$Bulma$Classes$isSuccess;
							case 9:
								return surprisetalk$elm_bulma$Bulma$Classes$isWarning;
							case 10:
								return surprisetalk$elm_bulma$Bulma$Classes$isDanger;
							default:
								return surprisetalk$elm_bulma$Bulma$Classes$isLink;
						}
					}(),
						function () {
						if (expanded) {
							return surprisetalk$elm_bulma$Bulma$Classes$isFullWidth;
						} else {
							return surprisetalk$elm_bulma$Bulma$Classes$none;
						}
					}()
					]),
				attrs_));
	});
var surprisetalk$elm_bulma$Bulma$Form$controlSelectModifiers = {b_: 0, h: false, au: elm$core$Maybe$Nothing, q: 1, bC: 0};
var surprisetalk$elm_bulma$Bulma$Classes$textarea = elm$html$Html$Attributes$class('textarea');
var surprisetalk$elm_bulma$Bulma$Form$controlTextArea = F3(
	function (mods, attrs, attrs_) {
		var size = mods.q;
		var state = mods.bC;
		var color = mods.b_;
		var readonly = mods.F;
		var disabled = mods.ao;
		var controlMods = {
			h: false,
			au: elm$core$Maybe$Nothing,
			av: elm$core$Maybe$Nothing,
			C: function () {
				if (state === 4) {
					return elm$core$Maybe$Just(size);
				} else {
					return elm$core$Maybe$Nothing;
				}
			}()
		};
		return A2(
			elm$core$Basics$composeL,
			A2(
				elm$core$Basics$composeL,
				A2(surprisetalk$elm_bulma$Bulma$Form$control, controlMods, attrs),
				surprisetalk$elm_bulma$Helpers$ls),
			A3(
				surprisetalk$elm_bulma$Helpers$node,
				'textarea',
				_List_fromArray(
					[
						surprisetalk$elm_bulma$Bulma$Classes$textarea,
						elm$html$Html$Attributes$disabled(disabled),
						function () {
						if (readonly) {
							return elm$html$Html$Attributes$readonly(readonly);
						} else {
							return surprisetalk$elm_bulma$Bulma$Classes$none;
						}
					}(),
						function () {
						switch (size) {
							case 0:
								return surprisetalk$elm_bulma$Bulma$Classes$isSmall;
							case 1:
								return surprisetalk$elm_bulma$Bulma$Classes$none;
							case 2:
								return surprisetalk$elm_bulma$Bulma$Classes$isMedium;
							default:
								return surprisetalk$elm_bulma$Bulma$Classes$isLarge;
						}
					}(),
						function () {
						switch (state) {
							case 1:
								return surprisetalk$elm_bulma$Bulma$Classes$isHovered;
							case 2:
								return surprisetalk$elm_bulma$Bulma$Classes$isFocused;
							case 3:
								return surprisetalk$elm_bulma$Bulma$Classes$isActive;
							case 4:
								return surprisetalk$elm_bulma$Bulma$Classes$isLoading;
							default:
								return surprisetalk$elm_bulma$Bulma$Classes$none;
						}
					}(),
						function () {
						switch (color) {
							case 0:
								return surprisetalk$elm_bulma$Bulma$Classes$none;
							case 1:
								return surprisetalk$elm_bulma$Bulma$Classes$isWhite;
							case 2:
								return surprisetalk$elm_bulma$Bulma$Classes$isLight;
							case 3:
								return surprisetalk$elm_bulma$Bulma$Classes$isDark;
							case 4:
								return surprisetalk$elm_bulma$Bulma$Classes$isBlack;
							case 5:
								return surprisetalk$elm_bulma$Bulma$Classes$isPrimary;
							case 7:
								return surprisetalk$elm_bulma$Bulma$Classes$isInfo;
							case 8:
								return surprisetalk$elm_bulma$Bulma$Classes$isSuccess;
							case 9:
								return surprisetalk$elm_bulma$Bulma$Classes$isWarning;
							case 10:
								return surprisetalk$elm_bulma$Bulma$Classes$isDanger;
							default:
								return surprisetalk$elm_bulma$Bulma$Classes$isLink;
						}
					}()
					]),
				attrs_));
	});
var surprisetalk$elm_bulma$Bulma$Form$controlTextAreaModifiers = {b_: 0, ao: false, F: false, q: 1, bC: 0};
var surprisetalk$elm_bulma$Bulma$Form$field = A2(
	surprisetalk$elm_bulma$Helpers$node,
	'div',
	_List_fromArray(
		[surprisetalk$elm_bulma$Bulma$Classes$field]));
var surprisetalk$elm_bulma$Bulma$Modifiers$Black = 4;
var surprisetalk$elm_bulma$Bulma$Modifiers$Light = 2;
var surprisetalk$elm_bulma$Bulma$Modifiers$Link = 6;
var surprisetalk$elm_bulma$Bulma$Modifiers$Primary = 5;
var surprisetalk$elm_bulma$Bulma$Modifiers$White = 1;
var surprisetalk$elm_bulma$Bulma$Modifiers$fullWidth = surprisetalk$elm_bulma$Bulma$Classes$isFullWidth;
var author$project$Page$Upload$exampleElementsAndComponents = A3(
	surprisetalk$elm_bulma$Bulma$Layout$section,
	0,
	_List_Nil,
	_List_fromArray(
		[
			A2(
			surprisetalk$elm_bulma$Bulma$Layout$container,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					author$project$Page$Upload$demoArticle,
					'Elements',
					_List_fromArray(
						[
							A3(
							author$project$Page$Upload$demoSection,
							'Form',
							_List_Nil,
							_List_fromArray(
								[
									A2(
									surprisetalk$elm_bulma$Bulma$Form$field,
									_List_Nil,
									_List_fromArray(
										[
											A2(
											surprisetalk$elm_bulma$Bulma$Form$controlLabel,
											_List_Nil,
											_List_fromArray(
												[
													elm$html$Html$text('Form Label')
												])),
											A4(
											surprisetalk$elm_bulma$Bulma$Form$controlInput,
											surprisetalk$elm_bulma$Bulma$Form$controlInputModifiers,
											_List_Nil,
											_List_fromArray(
												[
													elm$html$Html$Attributes$placeholder('Input')
												]),
											_List_Nil),
											A3(
											surprisetalk$elm_bulma$Bulma$Form$controlHelp,
											0,
											_List_Nil,
											_List_fromArray(
												[
													elm$html$Html$text('This field isn\'t required!')
												]))
										])),
									A2(
									surprisetalk$elm_bulma$Bulma$Form$field,
									_List_Nil,
									_List_fromArray(
										[
											A4(
											surprisetalk$elm_bulma$Bulma$Form$controlSelect,
											surprisetalk$elm_bulma$Bulma$Form$controlSelectModifiers,
											_List_Nil,
											_List_Nil,
											_List_fromArray(
												[
													A2(
													elm$html$Html$option,
													_List_Nil,
													_List_fromArray(
														[
															elm$html$Html$text('Dropdown')
														]))
												]))
										])),
									A2(
									surprisetalk$elm_bulma$Bulma$Form$field,
									_List_Nil,
									_List_fromArray(
										[
											A4(
											surprisetalk$elm_bulma$Bulma$Form$controlTextArea,
											surprisetalk$elm_bulma$Bulma$Form$controlTextAreaModifiers,
											_List_Nil,
											_List_fromArray(
												[
													elm$html$Html$Attributes$placeholder('Textarea')
												]),
											_List_Nil)
										])),
									A2(
									surprisetalk$elm_bulma$Bulma$Form$field,
									_List_Nil,
									_List_fromArray(
										[
											A5(
											surprisetalk$elm_bulma$Bulma$Form$controlCheckBox,
											false,
											_List_Nil,
											_List_Nil,
											_List_Nil,
											_List_fromArray(
												[
													elm$html$Html$text('Checkbox')
												]))
										])),
									A2(
									surprisetalk$elm_bulma$Bulma$Form$field,
									_List_Nil,
									_List_fromArray(
										[
											A2(
											surprisetalk$elm_bulma$Bulma$Form$controlRadio,
											_List_Nil,
											_List_fromArray(
												[
													A6(
													surprisetalk$elm_bulma$Bulma$Form$controlRadioButton,
													false,
													true,
													'yes',
													_List_Nil,
													_List_Nil,
													_List_fromArray(
														[
															elm$html$Html$text('Radio')
														])),
													A6(
													surprisetalk$elm_bulma$Bulma$Form$controlRadioButton,
													false,
													false,
													'no',
													_List_Nil,
													_List_Nil,
													_List_fromArray(
														[
															elm$html$Html$text('Button')
														]))
												]))
										])),
									A2(
									surprisetalk$elm_bulma$Bulma$Form$field,
									_List_Nil,
									_List_fromArray(
										[
											A4(
											surprisetalk$elm_bulma$Bulma$Form$controlButton,
											_Utils_update(
												surprisetalk$elm_bulma$Bulma$Elements$buttonModifiers,
												{b_: 6}),
											_List_Nil,
											_List_Nil,
											_List_fromArray(
												[
													elm$html$Html$text('Button')
												]))
										]))
								])),
							A3(
							author$project$Page$Upload$demoSection,
							'Box',
							_List_Nil,
							_List_fromArray(
								[
									A2(
									surprisetalk$elm_bulma$Bulma$Elements$box,
									_List_Nil,
									_List_fromArray(
										[author$project$Page$Upload$exampleMediaObject]))
								])),
							A3(
							author$project$Page$Upload$demoSection,
							'Button',
							_List_Nil,
							_List_fromArray(
								[
									A3(
									surprisetalk$elm_bulma$Bulma$Elements$buttons,
									0,
									_List_Nil,
									A2(
										elm$core$List$map,
										function (_n0) {
											var color = _n0.a;
											var label = _n0.b;
											return A3(
												surprisetalk$elm_bulma$Bulma$Elements$button,
												_Utils_update(
													surprisetalk$elm_bulma$Bulma$Elements$buttonModifiers,
													{b_: color}),
												_List_Nil,
												_List_fromArray(
													[
														elm$html$Html$text(label)
													]));
										},
										_List_fromArray(
											[
												_Utils_Tuple2(0, 'Default'),
												_Utils_Tuple2(1, 'White'),
												_Utils_Tuple2(2, 'Light'),
												_Utils_Tuple2(3, 'Dark'),
												_Utils_Tuple2(4, 'Black')
											]))),
									A3(
									surprisetalk$elm_bulma$Bulma$Elements$buttons,
									0,
									_List_Nil,
									A2(
										elm$core$List$map,
										function (_n1) {
											var color = _n1.a;
											var label = _n1.b;
											return A3(
												surprisetalk$elm_bulma$Bulma$Elements$button,
												_Utils_update(
													surprisetalk$elm_bulma$Bulma$Elements$buttonModifiers,
													{b_: color}),
												_List_Nil,
												_List_fromArray(
													[
														elm$html$Html$text(label)
													]));
										},
										_List_fromArray(
											[
												_Utils_Tuple2(5, 'Primary'),
												_Utils_Tuple2(6, 'Link'),
												_Utils_Tuple2(7, 'Info'),
												_Utils_Tuple2(8, 'Success'),
												_Utils_Tuple2(9, 'Warning'),
												_Utils_Tuple2(10, 'Danger')
											])))
								])),
							A3(
							author$project$Page$Upload$demoSection,
							'Notification',
							_List_Nil,
							_List_fromArray(
								[
									A4(
									surprisetalk$elm_bulma$Bulma$Elements$notificationWithDelete,
									5,
									_List_Nil,
									author$project$Page$Upload$NoOp,
									_List_fromArray(
										[
											elm$html$Html$text('Lorem ipsum dolor sit amet, consectetur adipiscing elit lorem ipsum dolor. Pellentesque risus mi, tempus quis placerat ut, porta nec nulla. Vestibulum rhoncus ac ex sit amet fringilla. Nullam gravida purus diam, et dictum felis venenatis efficitur. Sit amet, consectetur adipiscing elit')
										]))
								])),
							A3(
							author$project$Page$Upload$demoSection,
							'Progress Bar',
							_List_Nil,
							_List_fromArray(
								[
									A3(
									surprisetalk$elm_bulma$Bulma$Elements$easyProgress,
									_Utils_update(
										surprisetalk$elm_bulma$Bulma$Elements$progressModifiers,
										{b_: 5}),
									_List_Nil,
									0.4)
								])),
							A3(
							author$project$Page$Upload$demoSection,
							'Tags',
							_List_Nil,
							_List_fromArray(
								[
									A2(
									surprisetalk$elm_bulma$Bulma$Elements$tags,
									_List_Nil,
									A2(
										elm$core$List$map,
										function (_n2) {
											var color = _n2.a;
											var label = _n2.b;
											return A3(
												surprisetalk$elm_bulma$Bulma$Elements$tag,
												_Utils_update(
													surprisetalk$elm_bulma$Bulma$Elements$tagModifiers,
													{b_: color}),
												_List_Nil,
												_List_fromArray(
													[
														elm$html$Html$text(label)
													]));
										},
										_List_fromArray(
											[
												_Utils_Tuple2(5, 'Primary'),
												_Utils_Tuple2(6, 'Link'),
												_Utils_Tuple2(7, 'Info'),
												_Utils_Tuple2(8, 'Success'),
												_Utils_Tuple2(9, 'Warning'),
												_Utils_Tuple2(10, 'Danger')
											]))),
									A2(
									surprisetalk$elm_bulma$Bulma$Elements$tags,
									_List_Nil,
									A2(
										elm$core$List$map,
										function (_n3) {
											var color = _n3.a;
											var label = _n3.b;
											return A3(
												surprisetalk$elm_bulma$Bulma$Elements$tag,
												_Utils_update(
													surprisetalk$elm_bulma$Bulma$Elements$tagModifiers,
													{b_: color}),
												_List_Nil,
												_List_fromArray(
													[
														elm$html$Html$text(label)
													]));
										},
										_List_fromArray(
											[
												_Utils_Tuple2(0, 'Default'),
												_Utils_Tuple2(1, 'White'),
												_Utils_Tuple2(2, 'Light'),
												_Utils_Tuple2(3, 'Dark'),
												_Utils_Tuple2(4, 'Black')
											])))
								]))
						])),
					A2(
					author$project$Page$Upload$demoArticle,
					'Components',
					_List_fromArray(
						[
							A3(
							author$project$Page$Upload$demoSection,
							'Card',
							_List_Nil,
							_List_fromArray(
								[
									A3(
									surprisetalk$elm_bulma$Bulma$Columns$columns,
									surprisetalk$elm_bulma$Bulma$Columns$columnsModifiers,
									_List_Nil,
									_List_fromArray(
										[
											A3(
											surprisetalk$elm_bulma$Bulma$Columns$column,
											surprisetalk$elm_bulma$Bulma$Columns$columnModifiers,
											_List_Nil,
											_List_fromArray(
												[
													A2(
													surprisetalk$elm_bulma$Bulma$Components$card,
													_List_Nil,
													_List_fromArray(
														[
															A2(
															surprisetalk$elm_bulma$Bulma$Components$cardImage,
															_List_Nil,
															_List_fromArray(
																[
																	A3(
																	surprisetalk$elm_bulma$Bulma$Elements$image,
																	surprisetalk$elm_bulma$Bulma$Elements$FourByThree,
																	_List_Nil,
																	_List_fromArray(
																		[
																			A2(
																			elm$html$Html$img,
																			_List_fromArray(
																				[
																					elm$html$Html$Attributes$src('https://bulma.io/images/placeholders/1280x960.png')
																				]),
																			_List_Nil)
																		]))
																])),
															A2(
															surprisetalk$elm_bulma$Bulma$Components$cardContent,
															_List_Nil,
															_List_fromArray(
																[
																	A2(
																	surprisetalk$elm_bulma$Bulma$Layout$media,
																	_List_Nil,
																	_List_fromArray(
																		[
																			A2(
																			surprisetalk$elm_bulma$Bulma$Layout$mediaLeft,
																			_List_Nil,
																			_List_fromArray(
																				[
																					A3(
																					surprisetalk$elm_bulma$Bulma$Elements$image,
																					surprisetalk$elm_bulma$Bulma$Elements$OneByOne(3),
																					_List_Nil,
																					_List_fromArray(
																						[
																							A2(
																							elm$html$Html$img,
																							_List_fromArray(
																								[
																									elm$html$Html$Attributes$src('https://bulma.io/images/placeholders/96x96.png')
																								]),
																							_List_Nil)
																						]))
																				])),
																			A2(
																			surprisetalk$elm_bulma$Bulma$Layout$mediaContent,
																			_List_Nil,
																			A4(
																				surprisetalk$elm_bulma$Bulma$Elements$easyTitleWithSubtitle,
																				false,
																				3,
																				_List_fromArray(
																					[
																						elm$html$Html$text('John Smith')
																					]),
																				_List_fromArray(
																					[
																						elm$html$Html$text('@johnsmith')
																					])))
																		])),
																	A3(
																	surprisetalk$elm_bulma$Bulma$Elements$content,
																	1,
																	_List_Nil,
																	_List_fromArray(
																		[
																			elm$html$Html$text('Lorem ipsum dolor sit amet, consectetur adipiscing elit. Phasellus nec iaculis mauris.'),
																			A2(
																			elm$html$Html$a,
																			_List_Nil,
																			_List_fromArray(
																				[
																					elm$html$Html$text('@bulmaio')
																				])),
																			elm$html$Html$text('.'),
																			A2(
																			elm$html$Html$a,
																			_List_Nil,
																			_List_fromArray(
																				[
																					elm$html$Html$text('#css')
																				])),
																			elm$html$Html$text(' '),
																			A2(
																			elm$html$Html$a,
																			_List_Nil,
																			_List_fromArray(
																				[
																					elm$html$Html$text('#responsive')
																				])),
																			A2(
																			elm$html$Html$small,
																			_List_Nil,
																			_List_fromArray(
																				[
																					elm$html$Html$text('11:09 PM - 1 Jan 2016')
																				]))
																		]))
																]))
														]))
												])),
											A3(
											surprisetalk$elm_bulma$Bulma$Columns$column,
											surprisetalk$elm_bulma$Bulma$Columns$columnModifiers,
											_List_Nil,
											_List_fromArray(
												[
													A2(
													surprisetalk$elm_bulma$Bulma$Components$card,
													_List_Nil,
													_List_fromArray(
														[
															A2(
															surprisetalk$elm_bulma$Bulma$Components$cardContent,
															_List_Nil,
															A4(
																surprisetalk$elm_bulma$Bulma$Elements$easyTitleWithSubtitle,
																false,
																2,
																_List_fromArray(
																	[
																		elm$html$Html$text('“There are two hard things in computer science: cache invalidation, naming things, and off-by-one errors.”')
																	]),
																_List_fromArray(
																	[
																		elm$html$Html$text('Jeff Atwood')
																	]))),
															A2(
															surprisetalk$elm_bulma$Bulma$Components$cardFooter,
															_List_Nil,
															_List_fromArray(
																[
																	A2(
																	surprisetalk$elm_bulma$Bulma$Components$cardFooterItem,
																	_List_Nil,
																	_List_fromArray(
																		[
																			A2(
																			elm$html$Html$span,
																			_List_Nil,
																			_List_fromArray(
																				[
																					elm$html$Html$text('View on '),
																					A2(
																					elm$html$Html$a,
																					_List_Nil,
																					_List_fromArray(
																						[
																							elm$html$Html$text('Twitter')
																						]))
																				]))
																		])),
																	A2(
																	surprisetalk$elm_bulma$Bulma$Components$cardFooterItem,
																	_List_Nil,
																	_List_fromArray(
																		[
																			A2(
																			elm$html$Html$span,
																			_List_Nil,
																			_List_fromArray(
																				[
																					elm$html$Html$text('Share on '),
																					A2(
																					elm$html$Html$a,
																					_List_Nil,
																					_List_fromArray(
																						[
																							elm$html$Html$text('Facebook')
																						]))
																				]))
																		]))
																]))
														]))
												]))
										]))
								])),
							A3(
							author$project$Page$Upload$demoSection,
							'Dropdown',
							_List_fromArray(
								[
									A2(elm$html$Html$Attributes$style, 'height', '16rem')
								]),
							_List_fromArray(
								[
									A4(
									surprisetalk$elm_bulma$Bulma$Components$dropdown,
									true,
									surprisetalk$elm_bulma$Bulma$Components$dropdownModifiers,
									_List_Nil,
									_List_fromArray(
										[
											A2(
											surprisetalk$elm_bulma$Bulma$Components$dropdownTrigger,
											_List_Nil,
											_List_fromArray(
												[
													A3(
													surprisetalk$elm_bulma$Bulma$Elements$button,
													surprisetalk$elm_bulma$Bulma$Elements$buttonModifiers,
													_List_fromArray(
														[
															A2(elm$html$Html$Attributes$attribute, 'aria-haspopup', 'true'),
															A2(elm$html$Html$Attributes$attribute, 'aria-controls', 'dropdown-menu')
														]),
													_List_fromArray(
														[
															elm$html$Html$text('Dropdown button')
														]))
												])),
											A3(
											surprisetalk$elm_bulma$Bulma$Components$dropdownMenu,
											_List_Nil,
											_List_Nil,
											_List_fromArray(
												[
													A3(
													surprisetalk$elm_bulma$Bulma$Components$dropdownItem,
													false,
													_List_Nil,
													_List_fromArray(
														[
															elm$html$Html$text('Dropdown item')
														])),
													A3(
													surprisetalk$elm_bulma$Bulma$Components$dropdownItemLink,
													false,
													_List_Nil,
													_List_fromArray(
														[
															elm$html$Html$text('Other dropdown item')
														])),
													A3(
													surprisetalk$elm_bulma$Bulma$Components$dropdownItemLink,
													true,
													_List_Nil,
													_List_fromArray(
														[
															elm$html$Html$text('Active dropdown item')
														])),
													A3(
													surprisetalk$elm_bulma$Bulma$Components$dropdownItemLink,
													false,
													_List_Nil,
													_List_fromArray(
														[
															elm$html$Html$text('Other item')
														])),
													A2(surprisetalk$elm_bulma$Bulma$Components$dropdownDivider, _List_Nil, _List_Nil),
													A3(
													surprisetalk$elm_bulma$Bulma$Components$dropdownItemLink,
													false,
													_List_Nil,
													_List_fromArray(
														[
															elm$html$Html$text('With a divider')
														]))
												]))
										]))
								])),
							A3(
							author$project$Page$Upload$demoSection,
							'Message',
							_List_Nil,
							_List_fromArray(
								[
									A3(
									surprisetalk$elm_bulma$Bulma$Components$message,
									_Utils_update(
										surprisetalk$elm_bulma$Bulma$Components$messageModifiers,
										{b_: 5}),
									_List_Nil,
									_List_fromArray(
										[
											A3(
											surprisetalk$elm_bulma$Bulma$Components$messageHeaderWithDelete,
											_List_Nil,
											author$project$Page$Upload$NoOp,
											_List_fromArray(
												[
													elm$html$Html$text('Primary')
												])),
											A2(
											surprisetalk$elm_bulma$Bulma$Components$messageBody,
											_List_Nil,
											_List_fromArray(
												[
													elm$html$Html$text('Lorem ipsum dolor sit amet, consectetur adipiscing elit. Pellentesque risus mi, tempus quis placerat ut, porta nec nulla. Vestibulum rhoncus ac ex sit amet fringilla. Nullam gravida purus diam, et dictum felis venenatis efficitur. Aenean ac eleifend lacus, in mollis lectus. Donec sodales, arcu et sollicitudin porttitor, tortor urna tempor ligula, id porttitor mi magna a neque. Donec dui urna, vehicula et sem eget, facilisis sodales sem.')
												]))
										]))
								])),
							A3(
							author$project$Page$Upload$demoSection,
							'Pagination',
							_List_Nil,
							_List_fromArray(
								[
									A3(
									surprisetalk$elm_bulma$Bulma$Components$pagination,
									0,
									_List_Nil,
									_List_fromArray(
										[
											A2(
											surprisetalk$elm_bulma$Bulma$Components$paginationPrev,
											_List_Nil,
											_List_fromArray(
												[
													elm$html$Html$text('Previous')
												])),
											A2(
											surprisetalk$elm_bulma$Bulma$Components$paginationNext,
											_List_Nil,
											_List_fromArray(
												[
													elm$html$Html$text('Next')
												])),
											A2(
											surprisetalk$elm_bulma$Bulma$Components$paginationList,
											_List_Nil,
											_List_fromArray(
												[
													A3(
													surprisetalk$elm_bulma$Bulma$Components$paginationLink,
													false,
													_List_Nil,
													_List_fromArray(
														[
															elm$html$Html$text('1')
														])),
													A2(
													surprisetalk$elm_bulma$Bulma$Components$paginationEllipsis,
													_List_Nil,
													_List_fromArray(
														[
															elm$html$Html$text('…')
														])),
													A3(
													surprisetalk$elm_bulma$Bulma$Components$paginationLink,
													false,
													_List_Nil,
													_List_fromArray(
														[
															elm$html$Html$text('45')
														])),
													A3(
													surprisetalk$elm_bulma$Bulma$Components$paginationLink,
													true,
													_List_Nil,
													_List_fromArray(
														[
															elm$html$Html$text('46')
														])),
													A3(
													surprisetalk$elm_bulma$Bulma$Components$paginationLink,
													false,
													_List_Nil,
													_List_fromArray(
														[
															elm$html$Html$text('47')
														])),
													A2(
													surprisetalk$elm_bulma$Bulma$Components$paginationEllipsis,
													_List_Nil,
													_List_fromArray(
														[
															elm$html$Html$text('…')
														])),
													A3(
													surprisetalk$elm_bulma$Bulma$Components$paginationLink,
													false,
													_List_Nil,
													_List_fromArray(
														[
															elm$html$Html$text('83')
														]))
												]))
										]))
								])),
							A3(
							author$project$Page$Upload$demoSection,
							'Tabs',
							_List_Nil,
							_List_fromArray(
								[
									A4(
									surprisetalk$elm_bulma$Bulma$Components$tabs,
									_Utils_update(
										surprisetalk$elm_bulma$Bulma$Components$tabsModifiers,
										{cn: 1}),
									_List_Nil,
									_List_Nil,
									_List_fromArray(
										[
											A4(
											surprisetalk$elm_bulma$Bulma$Components$tab,
											true,
											_List_Nil,
											_List_Nil,
											_List_fromArray(
												[
													A3(
													surprisetalk$elm_bulma$Bulma$Elements$icon,
													1,
													_List_Nil,
													_List_fromArray(
														[
															A2(
															elm$html$Html$i,
															_List_fromArray(
																[
																	elm$html$Html$Attributes$class('fa fa-image')
																]),
															_List_Nil)
														])),
													elm$html$Html$text('Pictures')
												])),
											A4(
											surprisetalk$elm_bulma$Bulma$Components$tab,
											false,
											_List_Nil,
											_List_Nil,
											_List_fromArray(
												[
													A3(
													surprisetalk$elm_bulma$Bulma$Elements$icon,
													1,
													_List_Nil,
													_List_fromArray(
														[
															A2(
															elm$html$Html$i,
															_List_fromArray(
																[
																	elm$html$Html$Attributes$class('fa fa-music')
																]),
															_List_Nil)
														])),
													elm$html$Html$text('Music')
												])),
											A4(
											surprisetalk$elm_bulma$Bulma$Components$tab,
											false,
											_List_Nil,
											_List_Nil,
											_List_fromArray(
												[
													A3(
													surprisetalk$elm_bulma$Bulma$Elements$icon,
													1,
													_List_Nil,
													_List_fromArray(
														[
															A2(
															elm$html$Html$i,
															_List_fromArray(
																[
																	elm$html$Html$Attributes$class('fa fa-film')
																]),
															_List_Nil)
														])),
													elm$html$Html$text('Videos')
												])),
											A4(
											surprisetalk$elm_bulma$Bulma$Components$tab,
											false,
											_List_Nil,
											_List_Nil,
											_List_fromArray(
												[
													A3(
													surprisetalk$elm_bulma$Bulma$Elements$icon,
													1,
													_List_Nil,
													_List_fromArray(
														[
															A2(
															elm$html$Html$i,
															_List_fromArray(
																[
																	elm$html$Html$Attributes$class('fa fa-file-pdf-o')
																]),
															_List_Nil)
														])),
													elm$html$Html$text('Documents')
												]))
										]))
								])),
							A3(
							author$project$Page$Upload$demoSection,
							'Media Object',
							_List_Nil,
							_List_fromArray(
								[author$project$Page$Upload$exampleMediaObject])),
							A3(
							author$project$Page$Upload$demoSection,
							'Menu & Panel',
							_List_Nil,
							_List_fromArray(
								[
									A3(
									surprisetalk$elm_bulma$Bulma$Columns$columns,
									surprisetalk$elm_bulma$Bulma$Columns$columnsModifiers,
									_List_Nil,
									_List_fromArray(
										[
											A3(
											surprisetalk$elm_bulma$Bulma$Columns$column,
											surprisetalk$elm_bulma$Bulma$Columns$columnModifiers,
											_List_Nil,
											_List_fromArray(
												[
													A2(
													surprisetalk$elm_bulma$Bulma$Components$menu,
													_List_Nil,
													_List_fromArray(
														[
															A2(
															surprisetalk$elm_bulma$Bulma$Components$menuLabel,
															_List_Nil,
															_List_fromArray(
																[
																	elm$html$Html$text('General')
																])),
															A2(
															surprisetalk$elm_bulma$Bulma$Components$menuList,
															_List_Nil,
															_List_fromArray(
																[
																	A2(
																	surprisetalk$elm_bulma$Bulma$Components$menuListItem,
																	_List_Nil,
																	_List_fromArray(
																		[
																			A3(
																			surprisetalk$elm_bulma$Bulma$Components$menuListItemLink,
																			false,
																			_List_Nil,
																			_List_fromArray(
																				[
																					elm$html$Html$text('Dashboard')
																				]))
																		])),
																	A2(
																	surprisetalk$elm_bulma$Bulma$Components$menuListItem,
																	_List_Nil,
																	_List_fromArray(
																		[
																			A3(
																			surprisetalk$elm_bulma$Bulma$Components$menuListItemLink,
																			false,
																			_List_Nil,
																			_List_fromArray(
																				[
																					elm$html$Html$text('Customers')
																				]))
																		]))
																])),
															A2(
															surprisetalk$elm_bulma$Bulma$Components$menuLabel,
															_List_Nil,
															_List_fromArray(
																[
																	elm$html$Html$text('Administration')
																])),
															A2(
															surprisetalk$elm_bulma$Bulma$Components$menuList,
															_List_Nil,
															_List_fromArray(
																[
																	A2(
																	surprisetalk$elm_bulma$Bulma$Components$menuListItem,
																	_List_Nil,
																	_List_fromArray(
																		[
																			A3(
																			surprisetalk$elm_bulma$Bulma$Components$menuListItemLink,
																			false,
																			_List_Nil,
																			_List_fromArray(
																				[
																					elm$html$Html$text('Team Settings')
																				]))
																		])),
																	A2(
																	surprisetalk$elm_bulma$Bulma$Components$menuListItem,
																	_List_Nil,
																	_List_fromArray(
																		[
																			A3(
																			surprisetalk$elm_bulma$Bulma$Components$menuListItemLink,
																			true,
																			_List_Nil,
																			_List_fromArray(
																				[
																					elm$html$Html$text('Manage Your Team')
																				])),
																			A2(
																			surprisetalk$elm_bulma$Bulma$Components$menuList,
																			_List_Nil,
																			_List_fromArray(
																				[
																					A3(
																					surprisetalk$elm_bulma$Bulma$Components$menuListItemLink,
																					false,
																					_List_Nil,
																					_List_fromArray(
																						[
																							elm$html$Html$text('Members')
																						])),
																					A3(
																					surprisetalk$elm_bulma$Bulma$Components$menuListItemLink,
																					false,
																					_List_Nil,
																					_List_fromArray(
																						[
																							elm$html$Html$text('Plugins')
																						])),
																					A3(
																					surprisetalk$elm_bulma$Bulma$Components$menuListItemLink,
																					false,
																					_List_Nil,
																					_List_fromArray(
																						[
																							elm$html$Html$text('Add a member')
																						]))
																				]))
																		])),
																	A2(
																	surprisetalk$elm_bulma$Bulma$Components$menuListItem,
																	_List_Nil,
																	_List_fromArray(
																		[
																			A3(
																			surprisetalk$elm_bulma$Bulma$Components$menuListItemLink,
																			false,
																			_List_Nil,
																			_List_fromArray(
																				[
																					elm$html$Html$text('Invitations')
																				]))
																		])),
																	A2(
																	surprisetalk$elm_bulma$Bulma$Components$menuListItem,
																	_List_Nil,
																	_List_fromArray(
																		[
																			A3(
																			surprisetalk$elm_bulma$Bulma$Components$menuListItemLink,
																			false,
																			_List_Nil,
																			_List_fromArray(
																				[
																					elm$html$Html$text('Cloud Storage Environment Settings')
																				]))
																		])),
																	A2(
																	surprisetalk$elm_bulma$Bulma$Components$menuListItem,
																	_List_Nil,
																	_List_fromArray(
																		[
																			A3(
																			surprisetalk$elm_bulma$Bulma$Components$menuListItemLink,
																			false,
																			_List_Nil,
																			_List_fromArray(
																				[
																					elm$html$Html$text('Authentication')
																				]))
																		]))
																])),
															A2(
															surprisetalk$elm_bulma$Bulma$Components$menuLabel,
															_List_Nil,
															_List_fromArray(
																[
																	elm$html$Html$text('Transactions')
																])),
															A2(
															surprisetalk$elm_bulma$Bulma$Components$menuList,
															_List_Nil,
															_List_fromArray(
																[
																	A2(
																	surprisetalk$elm_bulma$Bulma$Components$menuListItem,
																	_List_Nil,
																	_List_fromArray(
																		[
																			A3(
																			surprisetalk$elm_bulma$Bulma$Components$menuListItemLink,
																			false,
																			_List_Nil,
																			_List_fromArray(
																				[
																					elm$html$Html$text('Payments')
																				]))
																		])),
																	A2(
																	surprisetalk$elm_bulma$Bulma$Components$menuListItem,
																	_List_Nil,
																	_List_fromArray(
																		[
																			A3(
																			surprisetalk$elm_bulma$Bulma$Components$menuListItemLink,
																			false,
																			_List_Nil,
																			_List_fromArray(
																				[
																					elm$html$Html$text('Transfers')
																				]))
																		])),
																	A2(
																	surprisetalk$elm_bulma$Bulma$Components$menuListItem,
																	_List_Nil,
																	_List_fromArray(
																		[
																			A3(
																			surprisetalk$elm_bulma$Bulma$Components$menuListItemLink,
																			false,
																			_List_Nil,
																			_List_fromArray(
																				[
																					elm$html$Html$text('Balance')
																				]))
																		]))
																]))
														]))
												])),
											A3(
											surprisetalk$elm_bulma$Bulma$Columns$column,
											surprisetalk$elm_bulma$Bulma$Columns$columnModifiers,
											_List_Nil,
											_List_fromArray(
												[
													A2(
													surprisetalk$elm_bulma$Bulma$Components$panel,
													_List_Nil,
													_List_fromArray(
														[
															A2(
															surprisetalk$elm_bulma$Bulma$Components$panelHeading,
															_List_Nil,
															_List_fromArray(
																[
																	elm$html$Html$text('Repositories')
																])),
															A3(
															surprisetalk$elm_bulma$Bulma$Components$panelBlock,
															false,
															_List_Nil,
															_List_fromArray(
																[
																	A4(surprisetalk$elm_bulma$Bulma$Form$controlInput, surprisetalk$elm_bulma$Bulma$Form$controlInputModifiers, _List_Nil, _List_Nil, _List_Nil)
																])),
															A2(
															surprisetalk$elm_bulma$Bulma$Components$panelTabs,
															_List_Nil,
															_List_fromArray(
																[
																	A3(
																	surprisetalk$elm_bulma$Bulma$Components$panelTab,
																	false,
																	_List_Nil,
																	_List_fromArray(
																		[
																			elm$html$Html$text('All')
																		])),
																	A3(
																	surprisetalk$elm_bulma$Bulma$Components$panelTab,
																	true,
																	_List_Nil,
																	_List_fromArray(
																		[
																			elm$html$Html$text('Public')
																		])),
																	A3(
																	surprisetalk$elm_bulma$Bulma$Components$panelTab,
																	true,
																	_List_Nil,
																	_List_fromArray(
																		[
																			elm$html$Html$text('Private')
																		])),
																	A3(
																	surprisetalk$elm_bulma$Bulma$Components$panelTab,
																	true,
																	_List_Nil,
																	_List_fromArray(
																		[
																			elm$html$Html$text('Sources')
																		])),
																	A3(
																	surprisetalk$elm_bulma$Bulma$Components$panelTab,
																	true,
																	_List_Nil,
																	_List_fromArray(
																		[
																			elm$html$Html$text('Forks')
																		]))
																])),
															A5(
															surprisetalk$elm_bulma$Bulma$Components$panelLinkWithIcon,
															true,
															_List_Nil,
															_List_Nil,
															_List_fromArray(
																[
																	A2(
																	elm$html$Html$i,
																	_List_fromArray(
																		[
																			elm$html$Html$Attributes$class('fa fa-book')
																		]),
																	_List_Nil)
																]),
															_List_fromArray(
																[
																	elm$html$Html$text('bulma')
																])),
															A5(
															surprisetalk$elm_bulma$Bulma$Components$panelLinkWithIcon,
															false,
															_List_Nil,
															_List_Nil,
															_List_fromArray(
																[
																	A2(
																	elm$html$Html$i,
																	_List_fromArray(
																		[
																			elm$html$Html$Attributes$class('fa fa-book')
																		]),
																	_List_Nil)
																]),
															_List_fromArray(
																[
																	elm$html$Html$text('marksheet')
																])),
															A5(
															surprisetalk$elm_bulma$Bulma$Components$panelLinkWithIcon,
															false,
															_List_Nil,
															_List_Nil,
															_List_fromArray(
																[
																	A2(
																	elm$html$Html$i,
																	_List_fromArray(
																		[
																			elm$html$Html$Attributes$class('fa fa-book')
																		]),
																	_List_Nil)
																]),
															_List_fromArray(
																[
																	elm$html$Html$text('minireset.css')
																])),
															A5(
															surprisetalk$elm_bulma$Bulma$Components$panelLinkWithIcon,
															false,
															_List_Nil,
															_List_Nil,
															_List_fromArray(
																[
																	A2(
																	elm$html$Html$i,
																	_List_fromArray(
																		[
																			elm$html$Html$Attributes$class('fa fa-book')
																		]),
																	_List_Nil)
																]),
															_List_fromArray(
																[
																	elm$html$Html$text('jgthms.github.io')
																])),
															A5(
															surprisetalk$elm_bulma$Bulma$Components$panelLinkWithIcon,
															false,
															_List_Nil,
															_List_Nil,
															_List_fromArray(
																[
																	A2(
																	elm$html$Html$i,
																	_List_fromArray(
																		[
																			elm$html$Html$Attributes$class('fa fa-code-fork')
																		]),
																	_List_Nil)
																]),
															_List_fromArray(
																[
																	elm$html$Html$text('daniellowtw/infBoard')
																])),
															A5(
															surprisetalk$elm_bulma$Bulma$Components$panelLinkWithIcon,
															false,
															_List_Nil,
															_List_Nil,
															_List_fromArray(
																[
																	A2(
																	elm$html$Html$i,
																	_List_fromArray(
																		[
																			elm$html$Html$Attributes$class('fa fa-code-fork')
																		]),
																	_List_Nil)
																]),
															_List_fromArray(
																[
																	elm$html$Html$text('mojs')
																])),
															A4(
															surprisetalk$elm_bulma$Bulma$Components$panelCheckbox,
															false,
															_List_Nil,
															_List_Nil,
															_List_fromArray(
																[
																	elm$html$Html$text('Remember Me')
																])),
															A3(
															surprisetalk$elm_bulma$Bulma$Components$panelBlock,
															false,
															_List_Nil,
															_List_fromArray(
																[
																	A3(
																	surprisetalk$elm_bulma$Bulma$Elements$button,
																	_Utils_update(
																		surprisetalk$elm_bulma$Bulma$Elements$buttonModifiers,
																		{b_: 6}),
																	_List_fromArray(
																		[surprisetalk$elm_bulma$Bulma$Modifiers$fullWidth]),
																	_List_fromArray(
																		[
																			elm$html$Html$text('Reset all filters')
																		]))
																]))
														]))
												]))
										]))
								]))
						]))
				]))
		]));
var author$project$Page$Upload$view = function (model) {
	return {
		b$: A2(
			elm$html$Html$main_,
			_List_Nil,
			_List_fromArray(
				[author$project$Page$Upload$exampleElementsAndComponents])),
		bG: 'Upload'
	};
};
var elm$core$Basics$always = F2(
	function (a, _n0) {
		return a;
	});
var elm$virtual_dom$VirtualDom$map = _VirtualDom_map;
var elm$html$Html$map = elm$virtual_dom$VirtualDom$map;
var author$project$Main$view = function (model) {
	var viewPage = F4(
		function (isMenuOpen, toggleMsg, toMsg, config) {
			var _n1 = A4(
				author$project$Page$view,
				isMenuOpen,
				toggleMsg,
				author$project$Session$viewer(
					author$project$Main$toSession(model)),
				config);
			var title = _n1.bG;
			var body = _n1.f;
			return {
				f: A2(
					elm$core$List$map,
					elm$html$Html$map(toMsg),
					body),
				bG: title
			};
		});
	switch (model.$) {
		case 0:
			return A4(
				viewPage,
				false,
				author$project$Main$Ignored,
				elm$core$Basics$always(author$project$Main$Ignored),
				author$project$Page$Blank$view);
		case 1:
			return A4(
				viewPage,
				false,
				author$project$Main$Ignored,
				elm$core$Basics$always(author$project$Main$Ignored),
				author$project$Page$NotFound$view);
		case 2:
			var dashboard = model.a;
			return A4(
				viewPage,
				dashboard.v,
				author$project$Page$Dashboard$ToggleMenuOpen,
				author$project$Main$GotDashboardMsg,
				author$project$Page$Dashboard$view(dashboard));
		case 3:
			var upload = model.a;
			return A4(
				viewPage,
				upload.v,
				author$project$Page$Upload$ToggleMenuOpen,
				author$project$Main$GotUploadMsg,
				author$project$Page$Upload$view(upload));
		case 4:
			var portfolio = model.a;
			return A4(
				viewPage,
				portfolio.v,
				author$project$Page$Portfolio$ToggleMenuOpen,
				author$project$Main$GotPortfolioMsg,
				author$project$Page$Portfolio$view(portfolio));
		case 5:
			var charts = model.a;
			return A4(
				viewPage,
				charts.v,
				author$project$Page$Charts$ToggleMenuOpen,
				author$project$Main$GotChartsMsg,
				author$project$Page$Charts$view(charts));
		case 6:
			var accbalance = model.a;
			return A4(
				viewPage,
				accbalance.v,
				author$project$Page$AccountBalance$ToggleMenuOpen,
				author$project$Main$GotAccountBalanceMsg,
				author$project$Page$AccountBalance$view(accbalance));
		case 7:
			var login = model.a;
			return A4(
				viewPage,
				login.v,
				author$project$Page$Login$ToggleMenuOpen,
				author$project$Main$GotLoginMsg,
				author$project$Page$Login$view(login));
		default:
			var apidoc = model.a;
			return A4(
				viewPage,
				apidoc.v,
				author$project$Page$ApiDocument$ToggleMenuOpen,
				author$project$Main$GotApiDocumentMsg,
				author$project$Page$ApiDocument$view(apidoc));
	}
};
var author$project$Main$main = A2(
	author$project$Api$application,
	author$project$Viewer$decoder,
	{be: author$project$Main$init, bl: author$project$Main$ChangedUrl, bm: author$project$Main$ClickedLink, bE: author$project$Main$subscriptions, bI: author$project$Main$update, bP: author$project$Main$view});
_Platform_export({'Main':{'init':author$project$Main$main(elm$json$Json$Decode$value)(0)}});}(this));var storageKey = 'store';
var flags = localStorage.getItem(storageKey);
var app = Elm.Main.init({flags: flags});

app.ports.storeCache.subscribe(function(val) {
 if (val === null) {
  localStorage.removeItem(storageKey);
 } else {
  localStorage.setItem(storageKey, JSON.stringify(val));
 }
 setTimeout(function() { app.ports.onStoreChange.send(val); }, 0);
});

window.addEventListener('storage', function(event) {
 if (event.storageArea === localStorage && event.key === storageKey) {
  app.ports.onStoreChange.send(event.newValue);
 }
}, false);

