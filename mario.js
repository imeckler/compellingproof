// This program was compiled from OCaml by js_of_ocaml 1.4
function caml_raise_with_arg (tag, arg) { throw [0, tag, arg]; }
function caml_raise_with_string (tag, msg) {
  caml_raise_with_arg (tag, new MlWrappedString (msg));
}
function caml_invalid_argument (msg) {
  caml_raise_with_string(caml_global_data[4], msg);
}
function caml_array_bound_error () {
  caml_invalid_argument("index out of bounds");
}
function caml_str_repeat(n, s) {
  if (!n) { return ""; }
  if (n & 1) { return caml_str_repeat(n - 1, s) + s; }
  var r = caml_str_repeat(n >> 1, s);
  return r + r;
}
function MlString(param) {
  if (param != null) {
    this.bytes = this.fullBytes = param;
    this.last = this.len = param.length;
  }
}
MlString.prototype = {
  string:null,
  bytes:null,
  fullBytes:null,
  array:null,
  len:null,
  last:0,
  toJsString:function() {
    return this.string = decodeURIComponent (escape(this.getFullBytes()));
  },
  toBytes:function() {
    if (this.string != null)
      var b = unescape (encodeURIComponent (this.string));
    else {
      var b = "", a = this.array, l = a.length;
      for (var i = 0; i < l; i ++) b += String.fromCharCode (a[i]);
    }
    this.bytes = this.fullBytes = b;
    this.last = this.len = b.length;
    return b;
  },
  getBytes:function() {
    var b = this.bytes;
    if (b == null) b = this.toBytes();
    return b;
  },
  getFullBytes:function() {
    var b = this.fullBytes;
    if (b !== null) return b;
    b = this.bytes;
    if (b == null) b = this.toBytes ();
    if (this.last < this.len) {
      this.bytes = (b += caml_str_repeat(this.len - this.last, '\0'));
      this.last = this.len;
    }
    this.fullBytes = b;
    return b;
  },
  toArray:function() {
    var b = this.bytes;
    if (b == null) b = this.toBytes ();
    var a = [], l = this.last;
    for (var i = 0; i < l; i++) a[i] = b.charCodeAt(i);
    for (l = this.len; i < l; i++) a[i] = 0;
    this.string = this.bytes = this.fullBytes = null;
    this.last = this.len;
    this.array = a;
    return a;
  },
  getArray:function() {
    var a = this.array;
    if (!a) a = this.toArray();
    return a;
  },
  getLen:function() {
    var len = this.len;
    if (len !== null) return len;
    this.toBytes();
    return this.len;
  },
  toString:function() { var s = this.string; return s?s:this.toJsString(); },
  valueOf:function() { var s = this.string; return s?s:this.toJsString(); },
  blitToArray:function(i1, a2, i2, l) {
    var a1 = this.array;
    if (a1) {
      if (i2 <= i1) {
        for (var i = 0; i < l; i++) a2[i2 + i] = a1[i1 + i];
      } else {
        for (var i = l - 1; i >= 0; i--) a2[i2 + i] = a1[i1 + i];
      }
    } else {
      var b = this.bytes;
      if (b == null) b = this.toBytes();
      var l1 = this.last - i1;
      if (l <= l1)
        for (var i = 0; i < l; i++) a2 [i2 + i] = b.charCodeAt(i1 + i);
      else {
        for (var i = 0; i < l1; i++) a2 [i2 + i] = b.charCodeAt(i1 + i);
        for (; i < l; i++) a2 [i2 + i] = 0;
      }
    }
  },
  get:function (i) {
    var a = this.array;
    if (a) return a[i];
    var b = this.bytes;
    if (b == null) b = this.toBytes();
    return (i<this.last)?b.charCodeAt(i):0;
  },
  safeGet:function (i) {
    if (this.len == null) this.toBytes();
    if ((i < 0) || (i >= this.len)) caml_array_bound_error ();
    return this.get(i);
  },
  set:function (i, c) {
    var a = this.array;
    if (!a) {
      if (this.last == i) {
        this.bytes += String.fromCharCode (c & 0xff);
        this.last ++;
        return 0;
      }
      a = this.toArray();
    } else if (this.bytes != null) {
      this.bytes = this.fullBytes = this.string = null;
    }
    a[i] = c & 0xff;
    return 0;
  },
  safeSet:function (i, c) {
    if (this.len == null) this.toBytes ();
    if ((i < 0) || (i >= this.len)) caml_array_bound_error ();
    this.set(i, c);
  },
  fill:function (ofs, len, c) {
    if (ofs >= this.last && this.last && c == 0) return;
    var a = this.array;
    if (!a) a = this.toArray();
    else if (this.bytes != null) {
      this.bytes = this.fullBytes = this.string = null;
    }
    var l = ofs + len;
    for (var i = ofs; i < l; i++) a[i] = c;
  },
  compare:function (s2) {
    if (this.string != null && s2.string != null) {
      if (this.string < s2.string) return -1;
      if (this.string > s2.string) return 1;
      return 0;
    }
    var b1 = this.getFullBytes ();
    var b2 = s2.getFullBytes ();
    if (b1 < b2) return -1;
    if (b1 > b2) return 1;
    return 0;
  },
  equal:function (s2) {
    if (this.string != null && s2.string != null)
      return this.string == s2.string;
    return this.getFullBytes () == s2.getFullBytes ();
  },
  lessThan:function (s2) {
    if (this.string != null && s2.string != null)
      return this.string < s2.string;
    return this.getFullBytes () < s2.getFullBytes ();
  },
  lessEqual:function (s2) {
    if (this.string != null && s2.string != null)
      return this.string <= s2.string;
    return this.getFullBytes () <= s2.getFullBytes ();
  }
}
function MlWrappedString (s) { this.string = s; }
MlWrappedString.prototype = new MlString();
function MlMakeString (l) { this.bytes = ""; this.len = l; }
MlMakeString.prototype = new MlString ();
function caml_array_get (array, index) {
  if ((index < 0) || (index >= array.length - 1)) caml_array_bound_error();
  return array[index+1];
}
function caml_array_set (array, index, newval) {
  if ((index < 0) || (index >= array.length - 1)) caml_array_bound_error();
  array[index+1]=newval; return 0;
}
function caml_blit_string(s1, i1, s2, i2, len) {
  if (len === 0) return;
  if (i2 === s2.last && s2.bytes != null) {
    var b = s1.bytes;
    if (b == null) b = s1.toBytes ();
    if (i1 > 0 || s1.last > len) b = b.slice(i1, i1 + len);
    s2.bytes += b;
    s2.last += b.length;
    return;
  }
  var a = s2.array;
  if (!a) a = s2.toArray(); else { s2.bytes = s2.string = null; }
  s1.blitToArray (i1, a, i2, len);
}
function caml_call_gen(f, args) {
  if(f.fun)
    return caml_call_gen(f.fun, args);
  var n = f.length;
  var d = n - args.length;
  if (d == 0)
    return f.apply(null, args);
  else if (d < 0)
    return caml_call_gen(f.apply(null, args.slice(0,n)), args.slice(n));
  else
    return function (x){ return caml_call_gen(f, args.concat([x])); };
}
function caml_classify_float (x) {
  if (isFinite (x)) {
    if (Math.abs(x) >= 2.2250738585072014e-308) return 0;
    if (x != 0) return 1;
    return 2;
  }
  return isNaN(x)?4:3;
}
function caml_create_string(len) {
  if (len < 0) caml_invalid_argument("String.create");
  return new MlMakeString(len);
}
function caml_int64_compare(x,y) {
  var x3 = x[3] << 16;
  var y3 = y[3] << 16;
  if (x3 > y3) return 1;
  if (x3 < y3) return -1;
  if (x[2] > y[2]) return 1;
  if (x[2] < y[2]) return -1;
  if (x[1] > y[1]) return 1;
  if (x[1] < y[1]) return -1;
  return 0;
}
function caml_int_compare (a, b) {
  if (a < b) return (-1); if (a == b) return 0; return 1;
}
function caml_compare_val (a, b, total) {
  var stack = [];
  for(;;) {
    if (!(total && a === b)) {
      if (a instanceof MlString) {
        if (b instanceof MlString) {
            if (a != b) {
		var x = a.compare(b);
		if (x != 0) return x;
	    }
        } else
          return 1;
      } else if (a instanceof Array && a[0] === (a[0]|0)) {
        var ta = a[0];
        if (ta === 250) {
          a = a[1];
          continue;
        } else if (b instanceof Array && b[0] === (b[0]|0)) {
          var tb = b[0];
          if (tb === 250) {
            b = b[1];
            continue;
          } else if (ta != tb) {
            return (ta < tb)?-1:1;
          } else {
            switch (ta) {
            case 248: {
		var x = caml_int_compare(a[2], b[2]);
		if (x != 0) return x;
		break;
	    }
            case 255: {
		var x = caml_int64_compare(a, b);
		if (x != 0) return x;
		break;
	    }
            default:
              if (a.length != b.length) return (a.length < b.length)?-1:1;
              if (a.length > 1) stack.push(a, b, 1);
            }
          }
        } else
          return 1;
      } else if (b instanceof MlString ||
                 (b instanceof Array && b[0] === (b[0]|0))) {
        return -1;
      } else {
        if (a < b) return -1;
        if (a > b) return 1;
        if (total && a != b) {
          if (a == a) return 1;
          if (b == b) return -1;
        }
      }
    }
    if (stack.length == 0) return 0;
    var i = stack.pop();
    b = stack.pop();
    a = stack.pop();
    if (i + 1 < a.length) stack.push(a, b, i + 1);
    a = a[i];
    b = b[i];
  }
}
function caml_equal (x, y) { return +(caml_compare_val(x,y,false) == 0); }
function caml_fill_string(s, i, l, c) { s.fill (i, l, c); }
function caml_parse_format (fmt) {
  fmt = fmt.toString ();
  var len = fmt.length;
  if (len > 31) caml_invalid_argument("format_int: format too long");
  var f =
    { justify:'+', signstyle:'-', filler:' ', alternate:false,
      base:0, signedconv:false, width:0, uppercase:false,
      sign:1, prec:-1, conv:'f' };
  for (var i = 0; i < len; i++) {
    var c = fmt.charAt(i);
    switch (c) {
    case '-':
      f.justify = '-'; break;
    case '+': case ' ':
      f.signstyle = c; break;
    case '0':
      f.filler = '0'; break;
    case '#':
      f.alternate = true; break;
    case '1': case '2': case '3': case '4': case '5':
    case '6': case '7': case '8': case '9':
      f.width = 0;
      while (c=fmt.charCodeAt(i) - 48, c >= 0 && c <= 9) {
        f.width = f.width * 10 + c; i++
      }
      i--;
     break;
    case '.':
      f.prec = 0;
      i++;
      while (c=fmt.charCodeAt(i) - 48, c >= 0 && c <= 9) {
        f.prec = f.prec * 10 + c; i++
      }
      i--;
    case 'd': case 'i':
      f.signedconv = true; /* fallthrough */
    case 'u':
      f.base = 10; break;
    case 'x':
      f.base = 16; break;
    case 'X':
      f.base = 16; f.uppercase = true; break;
    case 'o':
      f.base = 8; break;
    case 'e': case 'f': case 'g':
      f.signedconv = true; f.conv = c; break;
    case 'E': case 'F': case 'G':
      f.signedconv = true; f.uppercase = true;
      f.conv = c.toLowerCase (); break;
    }
  }
  return f;
}
function caml_finish_formatting(f, rawbuffer) {
  if (f.uppercase) rawbuffer = rawbuffer.toUpperCase();
  var len = rawbuffer.length;
  if (f.signedconv && (f.sign < 0 || f.signstyle != '-')) len++;
  if (f.alternate) {
    if (f.base == 8) len += 1;
    if (f.base == 16) len += 2;
  }
  var buffer = "";
  if (f.justify == '+' && f.filler == ' ')
    for (var i = len; i < f.width; i++) buffer += ' ';
  if (f.signedconv) {
    if (f.sign < 0) buffer += '-';
    else if (f.signstyle != '-') buffer += f.signstyle;
  }
  if (f.alternate && f.base == 8) buffer += '0';
  if (f.alternate && f.base == 16) buffer += "0x";
  if (f.justify == '+' && f.filler == '0')
    for (var i = len; i < f.width; i++) buffer += '0';
  buffer += rawbuffer;
  if (f.justify == '-')
    for (var i = len; i < f.width; i++) buffer += ' ';
  return new MlWrappedString (buffer);
}
function caml_format_float (fmt, x) {
  var s, f = caml_parse_format(fmt);
  var prec = (f.prec < 0)?6:f.prec;
  if (x < 0) { f.sign = -1; x = -x; }
  if (isNaN(x)) { s = "nan"; f.filler = ' '; }
  else if (!isFinite(x)) { s = "inf"; f.filler = ' '; }
  else
    switch (f.conv) {
    case 'e':
      var s = x.toExponential(prec);
      var i = s.length;
      if (s.charAt(i - 3) == 'e')
        s = s.slice (0, i - 1) + '0' + s.slice (i - 1);
      break;
    case 'f':
      s = x.toFixed(prec); break;
    case 'g':
      prec = prec?prec:1;
      s = x.toExponential(prec - 1);
      var j = s.indexOf('e');
      var exp = +s.slice(j + 1);
      if (exp < -4 || x.toFixed(0).length > prec) {
        var i = j - 1; while (s.charAt(i) == '0') i--;
        if (s.charAt(i) == '.') i--;
        s = s.slice(0, i + 1) + s.slice(j);
        i = s.length;
        if (s.charAt(i - 3) == 'e')
          s = s.slice (0, i - 1) + '0' + s.slice (i - 1);
        break;
      } else {
        var p = prec;
        if (exp < 0) { p -= exp + 1; s = x.toFixed(p); }
        else while (s = x.toFixed(p), s.length > prec + 1) p--;
        if (p) {
          var i = s.length - 1; while (s.charAt(i) == '0') i--;
          if (s.charAt(i) == '.') i--;
          s = s.slice(0, i + 1);
        }
      }
      break;
    }
  return caml_finish_formatting(f, s);
}
function caml_format_int(fmt, i) {
  if (fmt.toString() == "%d") return new MlWrappedString(""+i);
  var f = caml_parse_format(fmt);
  if (i < 0) { if (f.signedconv) { f.sign = -1; i = -i; } else i >>>= 0; }
  var s = i.toString(f.base);
  if (f.prec >= 0) {
    f.filler = ' ';
    var n = f.prec - s.length;
    if (n > 0) s = caml_str_repeat (n, '0') + s;
  }
  return caml_finish_formatting(f, s);
}
function caml_compare (a, b) { return caml_compare_val (a, b, true); }
function caml_greaterequal (x, y) { return +(caml_compare(x,y,false) >= 0); }
function caml_int64_is_negative(x) {
  return (x[3] << 16) < 0;
}
function caml_int64_neg (x) {
  var y1 = - x[1];
  var y2 = - x[2] + (y1 >> 24);
  var y3 = - x[3] + (y2 >> 24);
  return [255, y1 & 0xffffff, y2 & 0xffffff, y3 & 0xffff];
}
function caml_int64_of_int32 (x) {
  return [255, x & 0xffffff, (x >> 24) & 0xffffff, (x >> 31) & 0xffff]
}
function caml_int64_ucompare(x,y) {
  if (x[3] > y[3]) return 1;
  if (x[3] < y[3]) return -1;
  if (x[2] > y[2]) return 1;
  if (x[2] < y[2]) return -1;
  if (x[1] > y[1]) return 1;
  if (x[1] < y[1]) return -1;
  return 0;
}
function caml_int64_lsl1 (x) {
  x[3] = (x[3] << 1) | (x[2] >> 23);
  x[2] = ((x[2] << 1) | (x[1] >> 23)) & 0xffffff;
  x[1] = (x[1] << 1) & 0xffffff;
}
function caml_int64_lsr1 (x) {
  x[1] = ((x[1] >>> 1) | (x[2] << 23)) & 0xffffff;
  x[2] = ((x[2] >>> 1) | (x[3] << 23)) & 0xffffff;
  x[3] = x[3] >>> 1;
}
function caml_int64_sub (x, y) {
  var z1 = x[1] - y[1];
  var z2 = x[2] - y[2] + (z1 >> 24);
  var z3 = x[3] - y[3] + (z2 >> 24);
  return [255, z1 & 0xffffff, z2 & 0xffffff, z3 & 0xffff];
}
function caml_int64_udivmod (x, y) {
  var offset = 0;
  var modulus = x.slice ();
  var divisor = y.slice ();
  var quotient = [255, 0, 0, 0];
  while (caml_int64_ucompare (modulus, divisor) > 0) {
    offset++;
    caml_int64_lsl1 (divisor);
  }
  while (offset >= 0) {
    offset --;
    caml_int64_lsl1 (quotient);
    if (caml_int64_ucompare (modulus, divisor) >= 0) {
      quotient[1] ++;
      modulus = caml_int64_sub (modulus, divisor);
    }
    caml_int64_lsr1 (divisor);
  }
  return [0,quotient, modulus];
}
function caml_int64_to_int32 (x) {
  return x[1] | (x[2] << 24);
}
function caml_int64_is_zero(x) {
  return (x[3]|x[2]|x[1]) == 0;
}
function caml_int64_format (fmt, x) {
  var f = caml_parse_format(fmt);
  if (f.signedconv && caml_int64_is_negative(x)) {
    f.sign = -1; x = caml_int64_neg(x);
  }
  var buffer = "";
  var wbase = caml_int64_of_int32(f.base);
  var cvtbl = "0123456789abcdef";
  do {
    var p = caml_int64_udivmod(x, wbase);
    x = p[1];
    buffer = cvtbl.charAt(caml_int64_to_int32(p[2])) + buffer;
  } while (! caml_int64_is_zero(x));
  if (f.prec >= 0) {
    f.filler = ' ';
    var n = f.prec - buffer.length;
    if (n > 0) buffer = caml_str_repeat (n, '0') + buffer;
  }
  return caml_finish_formatting(f, buffer);
}
function caml_parse_sign_and_base (s) {
  var i = 0, base = 10, sign = s.get(0) == 45?(i++,-1):1;
  if (s.get(i) == 48)
    switch (s.get(i + 1)) {
    case 120: case 88: base = 16; i += 2; break;
    case 111: case 79: base =  8; i += 2; break;
    case  98: case 66: base =  2; i += 2; break;
    }
  return [i, sign, base];
}
function caml_parse_digit(c) {
  if (c >= 48 && c <= 57)  return c - 48;
  if (c >= 65 && c <= 90)  return c - 55;
  if (c >= 97 && c <= 122) return c - 87;
  return -1;
}
var caml_global_data = [0];
function caml_failwith (msg) {
  caml_raise_with_string(caml_global_data[3], msg);
}
function caml_int_of_string (s) {
  var r = caml_parse_sign_and_base (s);
  var i = r[0], sign = r[1], base = r[2];
  var threshold = -1 >>> 0;
  var c = s.get(i);
  var d = caml_parse_digit(c);
  if (d < 0 || d >= base) caml_failwith("int_of_string");
  var res = d;
  for (;;) {
    i++;
    c = s.get(i);
    if (c == 95) continue;
    d = caml_parse_digit(c);
    if (d < 0 || d >= base) break;
    res = base * res + d;
    if (res > threshold) caml_failwith("int_of_string");
  }
  if (i != s.getLen()) caml_failwith("int_of_string");
  res = sign * res;
  if ((res | 0) != res) caml_failwith("int_of_string");
  return res;
}
function caml_is_printable(c) { return +(c > 31 && c < 127); }
function caml_js_call(f, o, args) { return f.apply(o, args.slice(1)); }
function caml_js_eval_string () {return eval(arguments[0].toString());}
function caml_js_to_array(a) { return [0].concat(a); }
function caml_js_wrap_callback(f) {
  var toArray = Array.prototype.slice;
  return function () {
    var args = (arguments.length > 0)?toArray.call (arguments):[undefined];
    return caml_call_gen(f, args);
  }
}
function caml_js_wrap_meth_callback(f) {
  var toArray = Array.prototype.slice;
  return function () {
    var args = (arguments.length > 0)?toArray.call (arguments):[0];
    args.unshift (this);
    return caml_call_gen(f, args);
  }
}
function caml_make_vect (len, init) {
  var b = [0]; for (var i = 1; i <= len; i++) b[i] = init; return b;
}
function caml_ml_out_channels_list () { return 0; }
function caml_mul(x,y) {
  return ((((x >> 16) * y) << 16) + (x & 0xffff) * y)|0;
}
function caml_notequal (x, y) { return +(caml_compare_val(x,y,false) != 0); }
function caml_register_global (n, v) { caml_global_data[n + 1] = v; }
var caml_named_values = {};
function caml_register_named_value(nm,v) {
  caml_named_values[nm] = v; return 0;
}
function caml_string_equal(s1, s2) {
  var b1 = s1.fullBytes;
  var b2 = s2.fullBytes;
  if (b1 != null && b2 != null) return (b1 == b2)?1:0;
  return (s1.getFullBytes () == s2.getFullBytes ())?1:0;
}
function caml_string_notequal(s1, s2) { return 1-caml_string_equal(s1, s2); }
function caml_sys_const_word_size () { return 32; }
function caml_update_dummy (x, y) {
  if( typeof y==="function" ) { x.fun = y; return 0; }
  if( y.fun ) { x.fun = y.fun; return 0; }
  var i = y.length; while (i--) x[i] = y[i]; return 0;
}
(function(){function h_(sy,sz,sA,sB,sC,sD,sE){return sy.length==6?sy(sz,sA,sB,sC,sD,sE):caml_call_gen(sy,[sz,sA,sB,sC,sD,sE]);}function l2(ss,st,su,sv,sw,sx){return ss.length==5?ss(st,su,sv,sw,sx):caml_call_gen(ss,[st,su,sv,sw,sx]);}function dW(so,sp,sq,sr){return so.length==3?so(sp,sq,sr):caml_call_gen(so,[sp,sq,sr]);}function ep(sl,sm,sn){return sl.length==2?sl(sm,sn):caml_call_gen(sl,[sm,sn]);}function cY(sj,sk){return sj.length==1?sj(sk):caml_call_gen(sj,[sk]);}var a=[0,new MlString("Failure")],b=[0,new MlString("Invalid_argument")],c=[0,new MlString("Match_failure")],d=[0,600,400];caml_register_global(6,[0,new MlString("Not_found")]);caml_register_global(5,[0,new MlString("Division_by_zero")]);caml_register_global(3,b);caml_register_global(2,a);var b4=[0,new MlString("Assert_failure")],b3=new MlString("%d"),b2=new MlString("true"),b1=new MlString("false"),b0=new MlString("Pervasives.do_at_exit"),bZ=new MlString("\\b"),bY=new MlString("\\t"),bX=new MlString("\\n"),bW=new MlString("\\r"),bV=new MlString("\\\\"),bU=new MlString("\\'"),bT=new MlString("String.blit"),bS=new MlString("String.sub"),bR=new MlString("Buffer.add: cannot grow buffer"),bQ=new MlString(""),bP=new MlString(""),bO=new MlString("%.12g"),bN=new MlString("\""),bM=new MlString("\""),bL=new MlString("'"),bK=new MlString("'"),bJ=new MlString("nan"),bI=new MlString("neg_infinity"),bH=new MlString("infinity"),bG=new MlString("."),bF=new MlString("printf: bad positional specification (0)."),bE=new MlString("%_"),bD=[0,new MlString("printf.ml"),143,8],bC=new MlString("'"),bB=new MlString("Printf: premature end of format string '"),bA=new MlString("'"),bz=new MlString(" in format string '"),by=new MlString(", at char number "),bx=new MlString("Printf: bad conversion %"),bw=new MlString("Sformat.index_of_int: negative argument "),bv=new MlString("on"),bu=new MlString("canvas"),bt=new MlString("img"),bs=new MlString("2d"),br=new MlString("Dom_html.Canvas_not_available"),bq=new MlString("Option.value_exn: None"),bp=new MlString("(function(a,f){var len=a.length;for(var i = 0; i < len; ++i){f(a[i]);}})"),bo=new MlString("iter"),bn=new MlString("iter"),bm=new MlString("(function(t, x0, f){for(var k in t){if(t.hasOwnProperty(k)){x0=f(x0,parseInt(k),t[k]);}} return x0;})"),bl=new MlString("(function(t, f){for(var k in t){if(t.hasOwnProperty(k)){f(parseInt(k),t[k]);}}})"),bk=new MlString("(function(x,y){return x % y;})"),bj=new MlString("Stream.turn_on_derived: Listener was not off or passive"),bi=new MlString("Stream.turn_off_derived: Listener was not on"),bh=new MlString("Stream.Prim.turn_off: Listener was not on or passive"),bg=new MlString("Stream.Prim.turn_on: Listener was not off or passive"),bf=new MlString("rgba(%d,%d,%d,%f)"),be=new MlString("pageY"),bd=new MlString("pageX"),bc=new MlString("mousemove"),bb=new MlString("which"),ba=new MlString("key_stream.start"),a$=new MlString("keyup"),a_=new MlString("keydown"),a9=new MlString("body"),a8=new MlString("key_stream"),a7=new MlString("%dpx"),a6=new MlString("%f%%"),a5=new MlString("left"),a4=new MlString("right"),a3=new MlString("center"),a2=new MlString("justify"),a1=new MlString("initial"),a0=new MlString("inherit"),aZ=[0,[0,0,0],[0,1,0,0,1]],aY=new MlString("\""),aX=new MlString("'"),aW=new MlString("<"),aV=new MlString(">"),aU=new MlString("\n"),aT=new MlString("a"),aS=new MlString("src"),aR=[0,new MlString("src/oak_graphics.ml"),622,17],aQ=[0,new MlString("src/oak_graphics.ml"),600,38],aP=new MlString("div"),aO=new MlString("div"),aN=new MlString("relative"),aM=new MlString("hidden"),aL=new MlString("div"),aK=new MlString("none"),aJ=new MlString("left"),aI=new MlString("absolute"),aH=new MlString("translate%c(%dpx) "),aG=new MlString(""),aF=new MlString(""),aE=new MlString("absolute"),aD=new MlString("auto"),aC=new MlString("top"),aB=new MlString("bottom"),aA=new MlString("left"),az=new MlString("right"),ay=new MlString(""),ax=new MlString("transform"),aw=new MlString("msTransform"),av=new MlString("MozTransform"),au=new MlString("webkitTransform"),at=new MlString("OTransform"),as=new MlString("div"),ar=new MlString("visible"),aq=new MlString("auto"),ap=new MlString("%dpx"),ao=new MlString("%dpx"),an=new MlString("%dpx"),am=new MlString("%dpx"),al=new MlString("div"),ak=new MlString("hidden"),aj=new MlString("img"),ai=new MlString("cover"),ah=new MlString("BackgroundSize"),ag=new MlString("div"),af=new MlString("url(%s) no-repeat center"),ae=new MlString("webkit"),ad=new MlString("Moz"),ac=new MlString("O"),ab=new MlString("cover"),aa=new MlString("backgroundSize"),$=new MlString("div"),_=new MlString("url(%s)"),Z=new MlString("block"),Y=new MlString("%dpx"),X=new MlString("%dpx"),W=new MlString(""),V=new MlString(""),U=new MlString("%dpx"),T=new MlString("%dpx"),S=new MlString(""),R=new MlString(""),Q=new MlString("transparent"),P=new MlString(""),O=new MlString("a"),N=new MlString("100%"),M=new MlString("100%"),L=new MlString("0"),K=new MlString("0"),J=new MlString("block"),I=new MlString("absolute"),H=new MlString("auto"),G=new MlString("relative"),F=new MlString("auto"),E=new MlString("click"),D=new MlString("auto"),C=new MlString("mouseover"),B=new MlString("mouseout"),A=new MlString("0"),z=new MlString("0"),y=new MlString("Oak_graphics.draw"),x=new MlString("%dpx"),w=new MlString("%dpx"),v=new MlString("block"),u=new MlString("absolute"),t=new MlString("repeat"),s=new MlString("butt"),r=new MlString("round"),q=new MlString("square"),p=new MlString("round"),o=new MlString("bevel"),n=new MlString("miter"),m=new MlString(""),l=new MlString("left"),k=new MlString("right"),j=new MlString("content"),i=[0,0,0,0,0,new MlString("right")],h=new MlString("mario_changes");function g(e){throw [0,a,e];}function b5(f){throw [0,b,f];}function ce(b6,b8){var b7=b6.getLen(),b9=b8.getLen(),b_=caml_create_string(b7+b9|0);caml_blit_string(b6,0,b_,0,b7);caml_blit_string(b8,0,b_,b7,b9);return b_;}function cf(b$){return caml_format_int(b3,b$);}function cg(cd){var ca=caml_ml_out_channels_list(0);for(;;){if(ca){var cb=ca[2];try {}catch(cc){}var ca=cb;continue;}return 0;}}caml_register_named_value(b0,cg);function ct(ch,cj){var ci=caml_create_string(ch);caml_fill_string(ci,0,ch,cj);return ci;}function cu(cm,ck,cl){if(0<=ck&&0<=cl&&!((cm.getLen()-cl|0)<ck)){var cn=caml_create_string(cl);caml_blit_string(cm,ck,cn,0,cl);return cn;}return b5(bS);}function cv(cq,cp,cs,cr,co){if(0<=co&&0<=cp&&!((cq.getLen()-co|0)<cp)&&0<=cr&&!((cs.getLen()-co|0)<cr))return caml_blit_string(cq,cp,cs,cr,co);return b5(bT);}var cw=caml_sys_const_word_size(0),cx=caml_mul(cw/8|0,(1<<(cw-10|0))-1|0)-1|0;function cP(cy){var cz=1<=cy?cy:1,cA=cx<cz?cx:cz,cB=caml_create_string(cA);return [0,cB,0,cA,cB];}function cQ(cC){return cu(cC[1],0,cC[2]);}function cJ(cD,cF){var cE=[0,cD[3]];for(;;){if(cE[1]<(cD[2]+cF|0)){cE[1]=2*cE[1]|0;continue;}if(cx<cE[1])if((cD[2]+cF|0)<=cx)cE[1]=cx;else g(bR);var cG=caml_create_string(cE[1]);cv(cD[1],0,cG,0,cD[2]);cD[1]=cG;cD[3]=cE[1];return 0;}}function cR(cH,cK){var cI=cH[2];if(cH[3]<=cI)cJ(cH,1);cH[1].safeSet(cI,cK);cH[2]=cI+1|0;return 0;}function cS(cN,cL){var cM=cL.getLen(),cO=cN[2]+cM|0;if(cN[3]<cO)cJ(cN,cM);cv(cL,0,cN[1],cN[2],cM);cN[2]=cO;return 0;}function cW(cT){return 0<=cT?cT:g(ce(bw,cf(cT)));}function cX(cU,cV){return cW(cU+cV|0);}var cZ=cY(cX,1);function c6(c0){return cu(c0,0,c0.getLen());}function c8(c1,c2,c4){var c3=ce(bz,ce(c1,bA)),c5=ce(by,ce(cf(c2),c3));return b5(ce(bx,ce(ct(1,c4),c5)));}function d2(c7,c_,c9){return c8(c6(c7),c_,c9);}function d3(c$){return b5(ce(bB,ce(c6(c$),bC)));}function dy(da,di,dk,dm){function dh(db){if((da.safeGet(db)-48|0)<0||9<(da.safeGet(db)-48|0))return db;var dc=db+1|0;for(;;){var dd=da.safeGet(dc);if(48<=dd){if(!(58<=dd)){var df=dc+1|0,dc=df;continue;}var de=0;}else if(36===dd){var dg=dc+1|0,de=1;}else var de=0;if(!de)var dg=db;return dg;}}var dj=dh(di+1|0),dl=cP((dk-dj|0)+10|0);cR(dl,37);var dn=dm,dp=0;for(;;){if(dn){var dq=dn[2],dr=[0,dn[1],dp],dn=dq,dp=dr;continue;}var ds=dj,dt=dp;for(;;){if(ds<=dk){var du=da.safeGet(ds);if(42===du){if(dt){var dv=dt[2];cS(dl,cf(dt[1]));var dw=dh(ds+1|0),ds=dw,dt=dv;continue;}throw [0,b4,bD];}cR(dl,du);var dx=ds+1|0,ds=dx;continue;}return cQ(dl);}}}function fs(dE,dC,dB,dA,dz){var dD=dy(dC,dB,dA,dz);if(78!==dE&&110!==dE)return dD;dD.safeSet(dD.getLen()-1|0,117);return dD;}function d4(dL,dV,d0,dF,dZ){var dG=dF.getLen();function dX(dH,dU){var dI=40===dH?41:125;function dT(dJ){var dK=dJ;for(;;){if(dG<=dK)return cY(dL,dF);if(37===dF.safeGet(dK)){var dM=dK+1|0;if(dG<=dM)var dN=cY(dL,dF);else{var dO=dF.safeGet(dM),dP=dO-40|0;if(dP<0||1<dP){var dQ=dP-83|0;if(dQ<0||2<dQ)var dR=1;else switch(dQ){case 1:var dR=1;break;case 2:var dS=1,dR=0;break;default:var dS=0,dR=0;}if(dR){var dN=dT(dM+1|0),dS=2;}}else var dS=0===dP?0:1;switch(dS){case 1:var dN=dO===dI?dM+1|0:dW(dV,dF,dU,dO);break;case 2:break;default:var dN=dT(dX(dO,dM+1|0)+1|0);}}return dN;}var dY=dK+1|0,dK=dY;continue;}}return dT(dU);}return dX(d0,dZ);}function es(d1){return dW(d4,d3,d2,d1);}function eI(d5,ee,eo){var d6=d5.getLen()-1|0;function eq(d7){var d8=d7;a:for(;;){if(d8<d6){if(37===d5.safeGet(d8)){var d9=0,d_=d8+1|0;for(;;){if(d6<d_)var d$=d3(d5);else{var ea=d5.safeGet(d_);if(58<=ea){if(95===ea){var ec=d_+1|0,eb=1,d9=eb,d_=ec;continue;}}else if(32<=ea)switch(ea-32|0){case 1:case 2:case 4:case 5:case 6:case 7:case 8:case 9:case 12:case 15:break;case 0:case 3:case 11:case 13:var ed=d_+1|0,d_=ed;continue;case 10:var ef=dW(ee,d9,d_,105),d_=ef;continue;default:var eg=d_+1|0,d_=eg;continue;}var eh=d_;c:for(;;){if(d6<eh)var ei=d3(d5);else{var ej=d5.safeGet(eh);if(126<=ej)var ek=0;else switch(ej){case 78:case 88:case 100:case 105:case 111:case 117:case 120:var ei=dW(ee,d9,eh,105),ek=1;break;case 69:case 70:case 71:case 101:case 102:case 103:var ei=dW(ee,d9,eh,102),ek=1;break;case 33:case 37:case 44:case 64:var ei=eh+1|0,ek=1;break;case 83:case 91:case 115:var ei=dW(ee,d9,eh,115),ek=1;break;case 97:case 114:case 116:var ei=dW(ee,d9,eh,ej),ek=1;break;case 76:case 108:case 110:var el=eh+1|0;if(d6<el){var ei=dW(ee,d9,eh,105),ek=1;}else{var em=d5.safeGet(el)-88|0;if(em<0||32<em)var en=1;else switch(em){case 0:case 12:case 17:case 23:case 29:case 32:var ei=ep(eo,dW(ee,d9,eh,ej),105),ek=1,en=0;break;default:var en=1;}if(en){var ei=dW(ee,d9,eh,105),ek=1;}}break;case 67:case 99:var ei=dW(ee,d9,eh,99),ek=1;break;case 66:case 98:var ei=dW(ee,d9,eh,66),ek=1;break;case 41:case 125:var ei=dW(ee,d9,eh,ej),ek=1;break;case 40:var ei=eq(dW(ee,d9,eh,ej)),ek=1;break;case 123:var er=dW(ee,d9,eh,ej),et=dW(es,ej,d5,er),eu=er;for(;;){if(eu<(et-2|0)){var ev=ep(eo,eu,d5.safeGet(eu)),eu=ev;continue;}var ew=et-1|0,eh=ew;continue c;}default:var ek=0;}if(!ek)var ei=d2(d5,eh,ej);}var d$=ei;break;}}var d8=d$;continue a;}}var ex=d8+1|0,d8=ex;continue;}return d8;}}eq(0);return 0;}function gH(eJ){var ey=[0,0,0,0];function eH(eD,eE,ez){var eA=41!==ez?1:0,eB=eA?125!==ez?1:0:eA;if(eB){var eC=97===ez?2:1;if(114===ez)ey[3]=ey[3]+1|0;if(eD)ey[2]=ey[2]+eC|0;else ey[1]=ey[1]+eC|0;}return eE+1|0;}eI(eJ,eH,function(eF,eG){return eF+1|0;});return ey[1];}function fo(eK,eN,eL){var eM=eK.safeGet(eL);if((eM-48|0)<0||9<(eM-48|0))return ep(eN,0,eL);var eO=eM-48|0,eP=eL+1|0;for(;;){var eQ=eK.safeGet(eP);if(48<=eQ){if(!(58<=eQ)){var eT=eP+1|0,eS=(10*eO|0)+(eQ-48|0)|0,eO=eS,eP=eT;continue;}var eR=0;}else if(36===eQ)if(0===eO){var eU=g(bF),eR=1;}else{var eU=ep(eN,[0,cW(eO-1|0)],eP+1|0),eR=1;}else var eR=0;if(!eR)var eU=ep(eN,0,eL);return eU;}}function fj(eV,eW){return eV?eW:cY(cZ,eW);}function e_(eX,eY){return eX?eX[1]:eY;}function h9(g2,e0,hc,e3,gM,hi,eZ){var e1=cY(e0,eZ);function g3(e2){return ep(e3,e1,e2);}function gL(e8,hh,e4,fb){var e7=e4.getLen();function gI(g$,e5){var e6=e5;for(;;){if(e7<=e6)return cY(e8,e1);var e9=e4.safeGet(e6);if(37===e9){var ff=function(fa,e$){return caml_array_get(fb,e_(fa,e$));},fl=function(fn,fg,fi,fc){var fd=fc;for(;;){var fe=e4.safeGet(fd)-32|0;if(!(fe<0||25<fe))switch(fe){case 1:case 2:case 4:case 5:case 6:case 7:case 8:case 9:case 12:case 15:break;case 10:return fo(e4,function(fh,fm){var fk=[0,ff(fh,fg),fi];return fl(fn,fj(fh,fg),fk,fm);},fd+1|0);default:var fp=fd+1|0,fd=fp;continue;}var fq=e4.safeGet(fd);if(124<=fq)var fr=0;else switch(fq){case 78:case 88:case 100:case 105:case 111:case 117:case 120:var ft=ff(fn,fg),fu=caml_format_int(fs(fq,e4,e6,fd,fi),ft),fw=fv(fj(fn,fg),fu,fd+1|0),fr=1;break;case 69:case 71:case 101:case 102:case 103:var fx=ff(fn,fg),fy=caml_format_float(dy(e4,e6,fd,fi),fx),fw=fv(fj(fn,fg),fy,fd+1|0),fr=1;break;case 76:case 108:case 110:var fz=e4.safeGet(fd+1|0)-88|0;if(fz<0||32<fz)var fA=1;else switch(fz){case 0:case 12:case 17:case 23:case 29:case 32:var fB=fd+1|0,fC=fq-108|0;if(fC<0||2<fC)var fD=0;else{switch(fC){case 1:var fD=0,fE=0;break;case 2:var fF=ff(fn,fg),fG=caml_format_int(dy(e4,e6,fB,fi),fF),fE=1;break;default:var fH=ff(fn,fg),fG=caml_format_int(dy(e4,e6,fB,fi),fH),fE=1;}if(fE){var fI=fG,fD=1;}}if(!fD){var fJ=ff(fn,fg),fI=caml_int64_format(dy(e4,e6,fB,fi),fJ);}var fw=fv(fj(fn,fg),fI,fB+1|0),fr=1,fA=0;break;default:var fA=1;}if(fA){var fK=ff(fn,fg),fL=caml_format_int(fs(110,e4,e6,fd,fi),fK),fw=fv(fj(fn,fg),fL,fd+1|0),fr=1;}break;case 37:case 64:var fw=fv(fg,ct(1,fq),fd+1|0),fr=1;break;case 83:case 115:var fM=ff(fn,fg);if(115===fq)var fN=fM;else{var fO=[0,0],fP=0,fQ=fM.getLen()-1|0;if(!(fQ<fP)){var fR=fP;for(;;){var fS=fM.safeGet(fR),fT=14<=fS?34===fS?1:92===fS?1:0:11<=fS?13<=fS?1:0:8<=fS?1:0,fU=fT?2:caml_is_printable(fS)?1:4;fO[1]=fO[1]+fU|0;var fV=fR+1|0;if(fQ!==fR){var fR=fV;continue;}break;}}if(fO[1]===fM.getLen())var fW=fM;else{var fX=caml_create_string(fO[1]);fO[1]=0;var fY=0,fZ=fM.getLen()-1|0;if(!(fZ<fY)){var f0=fY;for(;;){var f1=fM.safeGet(f0),f2=f1-34|0;if(f2<0||58<f2)if(-20<=f2)var f3=1;else{switch(f2+34|0){case 8:fX.safeSet(fO[1],92);fO[1]+=1;fX.safeSet(fO[1],98);var f4=1;break;case 9:fX.safeSet(fO[1],92);fO[1]+=1;fX.safeSet(fO[1],116);var f4=1;break;case 10:fX.safeSet(fO[1],92);fO[1]+=1;fX.safeSet(fO[1],110);var f4=1;break;case 13:fX.safeSet(fO[1],92);fO[1]+=1;fX.safeSet(fO[1],114);var f4=1;break;default:var f3=1,f4=0;}if(f4)var f3=0;}else var f3=(f2-1|0)<0||56<(f2-1|0)?(fX.safeSet(fO[1],92),fO[1]+=1,fX.safeSet(fO[1],f1),0):1;if(f3)if(caml_is_printable(f1))fX.safeSet(fO[1],f1);else{fX.safeSet(fO[1],92);fO[1]+=1;fX.safeSet(fO[1],48+(f1/100|0)|0);fO[1]+=1;fX.safeSet(fO[1],48+((f1/10|0)%10|0)|0);fO[1]+=1;fX.safeSet(fO[1],48+(f1%10|0)|0);}fO[1]+=1;var f5=f0+1|0;if(fZ!==f0){var f0=f5;continue;}break;}}var fW=fX;}var fN=ce(bM,ce(fW,bN));}if(fd===(e6+1|0))var f6=fN;else{var f7=dy(e4,e6,fd,fi);try {var f8=0,f9=1;for(;;){if(f7.getLen()<=f9)var f_=[0,0,f8];else{var f$=f7.safeGet(f9);if(49<=f$)if(58<=f$)var ga=0;else{var f_=[0,caml_int_of_string(cu(f7,f9,(f7.getLen()-f9|0)-1|0)),f8],ga=1;}else{if(45===f$){var gc=f9+1|0,gb=1,f8=gb,f9=gc;continue;}var ga=0;}if(!ga){var gd=f9+1|0,f9=gd;continue;}}var ge=f_;break;}}catch(gf){if(gf[1]!==a)throw gf;var ge=c8(f7,0,115);}var gg=ge[1],gh=fN.getLen(),gi=0,gm=ge[2],gl=32;if(gg===gh&&0===gi){var gj=fN,gk=1;}else var gk=0;if(!gk)if(gg<=gh)var gj=cu(fN,gi,gh);else{var gn=ct(gg,gl);if(gm)cv(fN,gi,gn,0,gh);else cv(fN,gi,gn,gg-gh|0,gh);var gj=gn;}var f6=gj;}var fw=fv(fj(fn,fg),f6,fd+1|0),fr=1;break;case 67:case 99:var go=ff(fn,fg);if(99===fq)var gp=ct(1,go);else{if(39===go)var gq=bU;else if(92===go)var gq=bV;else{if(14<=go)var gr=0;else switch(go){case 8:var gq=bZ,gr=1;break;case 9:var gq=bY,gr=1;break;case 10:var gq=bX,gr=1;break;case 13:var gq=bW,gr=1;break;default:var gr=0;}if(!gr)if(caml_is_printable(go)){var gs=caml_create_string(1);gs.safeSet(0,go);var gq=gs;}else{var gt=caml_create_string(4);gt.safeSet(0,92);gt.safeSet(1,48+(go/100|0)|0);gt.safeSet(2,48+((go/10|0)%10|0)|0);gt.safeSet(3,48+(go%10|0)|0);var gq=gt;}}var gp=ce(bK,ce(gq,bL));}var fw=fv(fj(fn,fg),gp,fd+1|0),fr=1;break;case 66:case 98:var gv=fd+1|0,gu=ff(fn,fg)?b2:b1,fw=fv(fj(fn,fg),gu,gv),fr=1;break;case 40:case 123:var gw=ff(fn,fg),gx=dW(es,fq,e4,fd+1|0);if(123===fq){var gy=cP(gw.getLen()),gC=function(gA,gz){cR(gy,gz);return gA+1|0;};eI(gw,function(gB,gE,gD){if(gB)cS(gy,bE);else cR(gy,37);return gC(gE,gD);},gC);var gF=cQ(gy),fw=fv(fj(fn,fg),gF,gx),fr=1;}else{var gG=fj(fn,fg),gJ=cX(gH(gw),gG),fw=gL(function(gK){return gI(gJ,gx);},gG,gw,fb),fr=1;}break;case 33:cY(gM,e1);var fw=gI(fg,fd+1|0),fr=1;break;case 41:var fw=fv(fg,bQ,fd+1|0),fr=1;break;case 44:var fw=fv(fg,bP,fd+1|0),fr=1;break;case 70:var gN=ff(fn,fg);if(0===fi)var gO=bO;else{var gP=dy(e4,e6,fd,fi);if(70===fq)gP.safeSet(gP.getLen()-1|0,103);var gO=gP;}var gQ=caml_classify_float(gN);if(3===gQ)var gR=gN<0?bI:bH;else if(4<=gQ)var gR=bJ;else{var gS=caml_format_float(gO,gN),gT=0,gU=gS.getLen();for(;;){if(gU<=gT)var gV=ce(gS,bG);else{var gW=gS.safeGet(gT)-46|0,gX=gW<0||23<gW?55===gW?1:0:(gW-1|0)<0||21<(gW-1|0)?1:0;if(!gX){var gY=gT+1|0,gT=gY;continue;}var gV=gS;}var gR=gV;break;}}var fw=fv(fj(fn,fg),gR,fd+1|0),fr=1;break;case 91:var fw=d2(e4,fd,fq),fr=1;break;case 97:var gZ=ff(fn,fg),g0=cY(cZ,e_(fn,fg)),g1=ff(0,g0),g5=fd+1|0,g4=fj(fn,g0);if(g2)g3(ep(gZ,0,g1));else ep(gZ,e1,g1);var fw=gI(g4,g5),fr=1;break;case 114:var fw=d2(e4,fd,fq),fr=1;break;case 116:var g6=ff(fn,fg),g8=fd+1|0,g7=fj(fn,fg);if(g2)g3(cY(g6,0));else cY(g6,e1);var fw=gI(g7,g8),fr=1;break;default:var fr=0;}if(!fr)var fw=d2(e4,fd,fq);return fw;}},hb=e6+1|0,g_=0;return fo(e4,function(ha,g9){return fl(ha,g$,g_,g9);},hb);}ep(hc,e1,e9);var hd=e6+1|0,e6=hd;continue;}}function fv(hg,he,hf){g3(he);return gI(hg,hf);}return gI(hh,0);}var hj=ep(gL,hi,cW(0)),hk=gH(eZ);if(hk<0||6<hk){var hx=function(hl,hr){if(hk<=hl){var hm=caml_make_vect(hk,0),hp=function(hn,ho){return caml_array_set(hm,(hk-hn|0)-1|0,ho);},hq=0,hs=hr;for(;;){if(hs){var ht=hs[2],hu=hs[1];if(ht){hp(hq,hu);var hv=hq+1|0,hq=hv,hs=ht;continue;}hp(hq,hu);}return ep(hj,eZ,hm);}}return function(hw){return hx(hl+1|0,[0,hw,hr]);};},hy=hx(0,0);}else switch(hk){case 1:var hy=function(hA){var hz=caml_make_vect(1,0);caml_array_set(hz,0,hA);return ep(hj,eZ,hz);};break;case 2:var hy=function(hC,hD){var hB=caml_make_vect(2,0);caml_array_set(hB,0,hC);caml_array_set(hB,1,hD);return ep(hj,eZ,hB);};break;case 3:var hy=function(hF,hG,hH){var hE=caml_make_vect(3,0);caml_array_set(hE,0,hF);caml_array_set(hE,1,hG);caml_array_set(hE,2,hH);return ep(hj,eZ,hE);};break;case 4:var hy=function(hJ,hK,hL,hM){var hI=caml_make_vect(4,0);caml_array_set(hI,0,hJ);caml_array_set(hI,1,hK);caml_array_set(hI,2,hL);caml_array_set(hI,3,hM);return ep(hj,eZ,hI);};break;case 5:var hy=function(hO,hP,hQ,hR,hS){var hN=caml_make_vect(5,0);caml_array_set(hN,0,hO);caml_array_set(hN,1,hP);caml_array_set(hN,2,hQ);caml_array_set(hN,3,hR);caml_array_set(hN,4,hS);return ep(hj,eZ,hN);};break;case 6:var hy=function(hU,hV,hW,hX,hY,hZ){var hT=caml_make_vect(6,0);caml_array_set(hT,0,hU);caml_array_set(hT,1,hV);caml_array_set(hT,2,hW);caml_array_set(hT,3,hX);caml_array_set(hT,4,hY);caml_array_set(hT,5,hZ);return ep(hj,eZ,hT);};break;default:var hy=ep(hj,eZ,[0]);}return hy;}function h8(h0){return cP(2*h0.getLen()|0);}function h5(h3,h1){var h2=cQ(h1);h1[2]=0;return cY(h3,h2);}function ib(h4){var h7=cY(h5,h4);return h_(h9,1,h8,cR,cS,function(h6){return 0;},h7);}function ic(ia){return ep(ib,function(h$){return h$;},ia);}var id=[0,0];32===cw;var ie=null,ig=undefined;function ip(ih){var ii=ih==ie?0:[0,ih];return ii;}function io(ij){return ij;}function iq(ik){return ik!==ig?1:0;}function ir(il,im){return il!==ig?cY(im,il):0;}var is=false,it=Array,iw=RegExp,iv=Date;function ix(iu){return iu instanceof it?0:[0,new MlWrappedString(iu.toString())];}id[1]=[0,ix,id[1]];function iz(iy){return iy;}function i1(iA,iB){iA.appendChild(iB);return 0;}function iG(iC){return event;}function i2(iE){return iz(caml_js_wrap_callback(function(iD){if(iD){var iF=cY(iE,iD);if(!(iF|0))iD.preventDefault();return iF;}var iH=iG(0),iI=cY(iE,iH);if(!(iI|0))iH.returnValue=iI;return iI;}));}function i3(iJ,iK,iL){return iJ.call(iK,iL);}function i4(iM){return iM.toString();}function i5(iN,iO,iR,iY){if(iN.addEventListener===ig){var iP=bv.toString().concat(iO),iW=function(iQ){var iV=[0,iR,iQ,[0]];return cY(function(iU,iT,iS){return caml_js_call(iU,iT,iS);},iV);};iN.attachEvent(iP,iW);return function(iX){return iN.detachEvent(iP,iW);};}iN.addEventListener(iO,iR,iY);return function(iZ){return iN.removeEventListener(iO,iR,iY);};}function i6(i0){return cY(i0,0);}var i7=this.document,i$=bs.toString();function i_(i9,i8){return i9.createElement(i8.toString());}var ja=[0,br];this.HTMLElement===ig;function je(jb,jc){return jb?cY(jc,jb[1]):0;}function jv(jd){return jd?jd[1]:g(bq);}function ju(jg,jj){var jf=0,jh=jg.length-1-1|0;if(!(jh<jf)){var ji=jf;for(;;){cY(jj,jg[ji+1]);var jk=ji+1|0;if(jh!==ji){var ji=jk;continue;}break;}}return 0;}function jw(jl,jp,jr){var jm=jl?jl[1]:function(jo,jn){return caml_equal(jo,jn);},jq=jp.length-1-1|0;for(;;){if(0<=jq){if(!ep(jm,caml_array_get(jp,jq),jr)){var jt=jq-1|0,jq=jt;continue;}var js=1;}else var js=0;return js;}}var jx=caml_js_eval_string(bp);({}[bo.toString()]=jx);var jy=caml_js_eval_string(bm),jH={"iter":caml_js_eval_string(bl),"fold":jy};function jS(jz){return {};}function jT(jA,jB,jC){return jA[jB]=jC;}function jU(jD,jE){return delete jD[jE];}function jV(jF,jG){return jF.hasOwnProperty(jG)|0?[0,jF[jG]]:0;}function jQ(jM,jK){var jL=jH[bn.toString()];return jL(jM,caml_js_wrap_callback(function(jJ,jI){return ep(jK,jJ,jI);}));}function jW(jR){var jN=[0,0];jQ(jR,function(jO,jP){jN[1]+=1;return 0;});return jN[1];}caml_js_eval_string(bk);function j2(jX,jY){return window[jX.toString()]=jY;}function j3(jZ){return console.log(jZ.toString());}function j4(j0,j1){return cY(j1,j0);}function j9(j8,j5){return jQ(j8,function(j7,j6){return cY(j6,j5);});}function kl(ka,j_){return ju(ka,function(j$){return j9(j$,j_);});}function k8(ke,kb,kd){var kc=kb[6];kb[6]=kb[6]+1|0;jT(ke,kc,kd);return kc;}function lR(kf,kt){var kg=kf?kf[1]:function(kh,ki){return 0;},kj=jS(0),kk=jS(0);function kn(km){return kl([0,kj,kk],km);}var kr=0,kq=jS(0);function ks(ko){return 0;}return [0,[0,function(kp){return cY(kg,kn);},ks,kj,kq,kk,kr]];}function kH(kx,ku){{if(0===ku[0]){var kv=ku[1],kz=function(ky,kw){jT(kv[3],kx,kw);jU(ky,kx);return 1===jW(kv[3])?(kv[2]=cY(kv[1],0),0):0;},kA=jV(kv[4],kx);if(kA)var kB=kz(kv[4],kA[1]);else{var kC=jV(kv[5],kx),kB=kC?kz(kv[5],kC[1]):g(bg);}return kB;}var kD=ku[1],kJ=function(kF,kE){jT(kD[2],kx,kE);jU(kF,kx);if(1===jW(kD[2])){var kI=function(kG){return kH(kG[1],kG[2][1]);};return ju(kD[5],kI);}return 0;},kK=jV(kD[3],kx);if(kK)var kL=kJ(kD[3],kK[1]);else{var kM=jV(kD[4],kx),kL=kM?kJ(kD[4],kM[1]):g(bj);}return kL;}}function kZ(kQ,kN){{if(0===kN[0]){var kO=kN[1],kS=function(kR,kP){jT(kO[4],kQ,kP);return jU(kR,kQ);},kT=jV(kO[3],kQ);if(kT){kS(kO[3],kT[1]);var kU=0===jW(kO[3])?cY(kO[2],0):0;}else{var kV=jV(kO[5],kQ),kU=kV?kS(kO[5],kV[1]):g(bh);}return kU;}var kW=kN[1],kX=jV(kW[2],kQ);if(kX){jT(kW[3],kQ,kX[1]);jU(kW[2],kQ);if(0===jW(kW[2])){var k0=function(kY){return kZ(kY[1],kY[2][1]);},k1=ju(kW[5],k0);}else var k1=0;}else var k1=g(bi);return k1;}}function k$(k5,k2,k4){var k3=k2[1];k2[1]=k2[1]+1|0;jT(k5,k3,k4);return k3;}function lg(k6,k9){{if(0===k6[0]){var k7=k6[1];return k8(k7[4],k7,k9);}var k_=k6[1];return k$(k_[3],k_,k9);}}function lm(lh,lf){var la=jS(0),lb=jS(0);function le(lc){j9(la,lc);return j9(lb,lc);}var li=[0,[0,lg(lh,function(ld){return ep(lf,le,ld);}),[0,lh]]];return [1,[0,0,la,jS(0),lb,li]];}function lD(ln,lk){return lm(ln,function(ll,lj){return cY(ll,cY(lk,lj));});}function lS(lu,lr){var lo=[0,0];return lm(lu,function(ls,lp){function lt(lq){return cY(ls,ep(lr,lq,lp));}je(lo[1],lt);lo[1]=[0,lp];return 0;});}function lH(lv,ly){function lx(lw){lv[1]=lw;return 0;}if(0===ly[0]){var lz=ly[1],lA=k8(lz[5],lz,lx);}else{var lB=ly[1],lA=k$(lB[4],lB,lx);}return [0,ly,lv,[0,lA]];}function lT(lC,lE){var lF=lC[2],lG=lD(lC[1],lE);return lH([0,cY(lE,lF[1])],lG);}function lU(lI){return lI[1];}function lV(lQ,lJ,lM){var lK=[0,lJ],lP=[0,lJ];return lH(lP,lm(lQ,function(lO,lL){var lN=ep(lM,lK[1],lL);lK[1]=lN;return cY(lO,lN);}));}function l3(lW,l0,lZ,lY){var lX=lW?lW[1]:1;return [0,l0,lZ,lY,lX];}function l4(l1){return l2(ic,bf,l1[1],l1[2],l1[3],l1[4]);}function l6(l5){return l5;}var l7=jQuery(a9.toString());function mk(mh,l_){function l9(l8){return [0,l8[1],caml_js_wrap_callback(l8[2])];}var l$=l_.length-1;if(0===l$)var ma=[0];else{var mb=caml_make_vect(l$,l9(l_[0+1])),mc=1,md=l$-1|0;if(!(md<mc)){var me=mc;for(;;){mb[me+1]=l9(l_[me+1]);var mf=me+1|0;if(md!==me){var me=mf;continue;}break;}}var ma=mb;}ju(ma,function(mg){return mh.on(mg[1].toString(),mg[2]);});return function(mj){return ju(ma,function(mi){return mh.off(mi[1].toString(),mi[2]);});};}var mr=0,ms=lR([0,function(mo){j3(ba);function mn(ml){return ml[bb.toString()];}var mq=[0,a$,function(mm){mm.preventDefault();return cY(mo,[0,19067,mn(mm)]);}];return mk(l7,[0,[0,a_,function(mp){mp.preventDefault();return cY(mo,[0,759637122,mn(mp)]);}],mq]);}],mr);j2(a8,ms);var mt=jS(0),mF=lV(ms,[0],function(my,mu){if(759637122<=mu[1])jT(mt,mu[2],0);else jU(mt,mu[2]);var mv=new it();jQ(mt,function(mw,mx){return mv.push(mw);});return caml_js_to_array(mv);});function mB(mz){return 0===mz?0:1;}var mG=lT(mF,function(mA){var mC=mB(jw(0,mA,l6(40))),mD=mB(jw(0,mA,l6(38)))-mC|0,mE=mB(jw(0,mA,l6(37)));return [0,mB(jw(0,mA,l6(39)))-mE|0,mD];}),mJ=0,mM=lR([0,function(mI){return mk(l7,[0,[0,bc,function(mH){return cY(mI,[0,mH[bd.toString()],mH[be.toString()]]);}]]);}],mJ);lS(mM,function(mL,mK){return [0,mK[1]-mL[1]|0,mK[2]-mL[2]|0];});function mR(mN,mP){var mO=mN/2,mQ=mP/2;return [0,[0,-mO,-mQ],[0,-mO,mQ],[0,mO,mQ],[0,mO,-mQ]];}var mW=4*Math.atan(1);function mV(mS){var mT=0===mS[0]?ep(ic,a7,mS[1]):ep(ic,a6,mS[1]);return mT.toString();}function mX(mU){return new iw(mU.toString());}mX(aY);mX(aX);mX(aW);mX(aV);mX(aU);function m5(mY){return Math.round(mY);}function m2(mZ){return [0,0,1,0,0,1,mZ];}function m6(m0,m1){return m2([1,[1,[0,m0]],m1]);}function m7(m3,m4){return [0,m4[1],m4[2],m4[3]+m3[1],m4[4]+m3[2],m4[5],m4[6]];}[0,0][1]+=1;function np(m_,m8,ne){if(1<m8.length-1){var m9=caml_array_get(m8,0);m_.moveTo(m9[1],m9[2]);var m$=1,na=m8.length-1-1|0;if(!(na<m$)){var nb=m$;for(;;){var nc=caml_array_get(m8,nb);m_.lineTo(nc[1],nc[2]);var nd=nb+1|0;if(na!==nb){var nb=nd;continue;}break;}}if(ne){var nf=caml_array_get(m8,0);return m_.lineTo(nf[1],nf[2]);}return 0;}return 0;}function nD(nh,ng,nr,nq){nh.lineWidth=ng[2];var ni=ng[3],nj=416330352===ni?q:781662169<=ni?s:r;nh.lineCap=nj.toString();var nk=ng[4],nl=typeof nk==="number"?1006599246<=nk?p:o:n;nh.lineJoin=nl.toString();var nm=ng[4];if(typeof nm==="number"||!(256529610===nm[1]))var nn=0;else{var no=nm[2],nn=1;}if(!nn)var no=10;nh.miterLimit=no;nh.strokeStyle=l4(ng[1]).toString();if(0===ng[5].length-1)np(nh,nr,nq);else if(1<nr.length-1){var ns=caml_array_get(nr,nr.length-1-1|0);nh.moveTo(ns[1],ns[2]);g(m);}nh.scale(1,-1);return nh.stroke();}function n1(nA,nt){var nu=nt[6],nv=nt[5],nw=nt[4],nx=nt[3],ny=nt[2],nz=nt[1];nA.save();var nB=nx!=0?0:nw!=0?0:1;if(!nB)nA.translate(nx,nw);if(nz!=0)nA.rotate(nz);if(ny!=1)nA.scale(ny,ny);if(nv!=1)nA.globalAlpha=nA.globalAlpha*nv;nA.beginPath();switch(nu[0]){case 1:var nC=nu[1];if(0===nC[0])nD(nA,nC[1],nu[2],1);else{var nE=nC[1];np(nA,nu[2],0);switch(nE[0]){case 1:nA.fillStyle=nA.createPattern(nE[1],t.toString());break;case 2:var nF=nE[1];if(0===nF[0]){var nG=nF[2],nH=nF[1],nI=nF[3],nJ=[0,nA.createLinearGradient(nH[1],-nH[2],nG[1],-nG[2]),nI];}else{var nK=nF[3],nL=nF[1],nM=nF[5],nJ=[0,nA.createRadialGradient(nL[1],-nL[2],nF[2],nK[1],-nK[2],nF[4]),nM];}var nN=nJ[1],nQ=nJ[2];ju(nQ,function(nO){var nP=nO[1];return nN.addColorStop(nP,l4(nO[2]).toString());});nA.fillStyle=nN;break;default:nA.fillStyle=l4(nE[1]).toString();}nA.scale(1,-1);nA.fill();}break;case 2:var nR=nu[3],nS=nu[2],nT=nu[1],nW=nu[4],nV=nR[2],nU=nR[1];nA.scale(1,-1);nA.drawImage(nW,nU,nV,nT,nS,-nT/2,-nS/2,nT,nS);break;case 3:var nX=nu[1],nY=nX[2],nZ=nX[1],n0=nu[2];nA.transform(nY[1],nY[2],nY[3],nY[4],nZ[1],nZ[2]);ju(n0,cY(n1,nA));break;default:nD(nA,nu[1],nu[2],0);}return nA.restore();}function oo(n2){var n3=i7.createElement(n2.toString());n3.style.padding=A.toString();n3.style.margin=z.toString();return n3;}function o0(n4,n5){n4.style.pointerEvents=D.toString();n4.oakHoverHandler=io(n5);n4.hoverTriggered=0;function n$(n7,n8){return i2(function(n6){if(!cY(n7,n6)){n4.hoverTriggered=n8;var n_=function(n9){i3(n9,0,n6);return 0;};ir(n4.oakHoverHandler,n_);n6.stopPropagation();}return is;});}var ob=1,oe=n$(function(oa){return n4.hoverTriggered;},ob),od=0,of=n$(function(oc){return n4.contains(oc.toElement);},od),og=i5({},i4(C),oe,is),oh=i5({},i4(B),of,is);n4.hoverOverId=io(og);return n4.hoverOutId=io(oh);}function o1(oi,oj){oi.style.pointerEvents=F.toString();oi.oakClickHandler=io(oj);var on=i2(function(ok){function om(ol){i3(ol,0,ok);return 0;}ir(oi.oakClickHandler,om);return ok.stopPropagation();});return oi.clickId=io(i5({},i4(E),on,is));}function o2(os,or){var op=oo(O),oq=op.style;op.href=or.toString();oq.width=N.toString();oq.height=M.toString();oq.top=L.toString();oq.left=K.toString();oq.display=J.toString();oq.position=I.toString();oq.pointerEvents=H.toString();os.style.position=G.toString();return i1(os,op);}function oJ(ou,ot,ov){return ou[ot.toString()]=ov;}function o3(oy,ow,oz){var ox=ow[1],oA=oz.style,oG=ox[3],oF=ox[2],oE=oy[4],oD=oy[3],oC=oy[2],oB=oy[1];oA.position=aE.toString();oA.margin=aD.toString();function oO(oH,oN,oK,oL,oM,oI){return 80===oH?(oJ(oA,oK,mV(oI)),oA.removeProperty(oL.toString()),aF):90<=oH?dW(ic,aH,oN,(-oM|0)/2|0):(oJ(oA,oL,mV(oI)),oA.removeProperty(oL.toString()),aG);}var oP=oO(oC,89,aB,aC,oG,oE),oQ=ce(oO(oB,88,az,aA,oF,oD),oP);if(caml_string_notequal(oQ,ay)){var oR=oQ.toString();oJ(oA,ax,oR);oJ(oA,aw,oR);oJ(oA,av,oR);oJ(oA,au,oR);return oJ(oA,at,oR);}return 0;}function oY(oS){return oS;}function oW(oT){oT.style.position=aI.toString();return oT;}function oX(oU){oU.style.cssFloat=aJ.toString();return oU;}function o4(oV){return 2<=oV?4<=oV?oW:oX:oY;}function o5(oZ){switch(oZ){case 0:case 2:case 4:return 1;default:return 0;}}var o6=[],o7=[],o8=[],o9=[];caml_update_dummy(o6,function(o$,pd){var o_=oo(aL);if(5<=o$)o_.style.pointerEvents=aK.toString();var pb=o4(o$);function pc(pa){return i1(o_,cY(pb,cY(o9,pa)));}if(o5(o$)){var pe=pd.length-1-1|0;for(;;){if(0<=pe){pc(caml_array_get(pd,pe));var pf=pe-1|0,pe=pf;continue;}break;}}else ju(pd,pc);return o_;});caml_update_dummy(o7,function(pi,pg){var ph=cY(o9,pg);o3(pi,pg,ph);var pj=oo(aO);pj.style.position=aN.toString();pj.style.overflow=aM.toString();i1(pj,ph);return pj;});caml_update_dummy(o8,function(pk){var pl=pk[2],pm=pk[1];if(typeof pl==="number")return oo(aP);else switch(pl[0]){case 0:var pn=pl[4],po=pl[1],pH=pl[3],pE=pl[2],pG=pm[3],pD=pm[2];if(typeof po==="number")switch(po){case 1:var pp=oo(ag),pt=function(pq){var pr=ai.toString(),ps=ce(pq,ah);return oJ(pp.style,ps,pr);},pu=pp.style;pu.background=ep(ic,af,pn).toString();pt(ae);pt(ad);pt(ac);oJ(pp.style,aa,ab.toString());var pv=pp;break;case 2:var pw=oo($),px=pw.style;px.backgroundImage=ep(ic,_,pn).toString();var pv=pw;break;default:var py=i_(i7,bt);py.src=pn.toString();py.name=pn.toString();py.style.display=Z.toString();var pv=py;}else{var pB=po[2],pA=po[1],pz=oo(al);pz.style.overflow=ak.toString();var pC=oo(aj),pP=function(pJ,pO){var pF=pD/pE,pI=pG/pH,pK=pC.style;pK.width=ep(ic,ap,m5(pJ.width*pF)).toString();var pL=pC.style;pL.width=ep(ic,ao,m5(pJ.height*pI)).toString();var pM=pC.style;pM.marginLeft=ep(ic,an,m5((-pA|0)*pF)).toString();var pN=pC.style;pN.marginTop=ep(ic,am,m5((-pB|0)*pI)).toString();return is;};pC.onload=iz(caml_js_wrap_meth_callback(function(pR,pQ){if(pQ){var pS=pP(pR,pQ);if(!(pS|0))pQ.preventDefault();return pS;}var pT=iG(0),pU=pP(pR,pT);if(!(pU|0))pT.returnValue=pU;return pU;}));pC.src=pn.toString();pC.name=pn.toString();i1(pz,pC);var pv=pz;}return pv;case 1:return ep(o7,pl[1],pl[2]);case 2:return ep(o6,pl[1],pl[2]);case 3:var pX=pl[2],pW=pl[1],pV=oo(as),pY=pV.style;pV.innerHTML=pW.toString();je(pX,function(pZ){switch(pZ){case 1:var p0=a4;break;case 2:var p0=a3;break;case 3:var p0=a2;break;case 4:var p0=a1;break;case 5:var p0=a0;break;default:var p0=a5;}return pY.textAlign=p0.toString();});pY.visibility=ar.toString();pY.pointerEvents=aq.toString();return pV;default:throw [0,c,aQ];}});caml_update_dummy(o9,function(p1){var p2=cY(o8,p1),p3=p1[1],p4=p3[7],p5=p3[6],p6=p3[4],p$=p3[9],p_=p3[8],p9=p3[5],p8=p3[3],p7=p2.style;p7.width=ep(ic,Y,p3[2]).toString();var qa=p2.style;qa.height=ep(ic,X,p8).toString();if(p6!=1){var qb=p2.style;qb.opacity=io(p6.toString());}je(p9,function(qc){var qd=p2.style;return qd.backgroundColor=l4(qc).toString();});if(caml_string_notequal(p4,W))p2.id=p4.toString();if(caml_string_notequal(p5,V))o2(p2,p5);je(p_,cY(o0,p2));je(p$,cY(o1,p2));return p2;});var qe=[];function qK(qg,qf){var qh=cY(o9,qf);qg.parentNode.replaceChild(qh,qg);return 0;}caml_update_dummy(qe,function(qi,ql,qk){var qj=caml_equal(qi.tagName,aT.toString())?qi.firstChild:qi;if(ql[1][1]===qk[1][1]){if(qk[1][2]!==ql[1][2]){var qm=qj.style;qm.width=ep(ic,U,qk[1][2]).toString();}if(qk[1][3]!==ql[1][3]){var qn=qj.style;qn.height=ep(ic,T,qk[1][3]).toString();}if(qk[1][4]!=ql[1][4]){var qo=qj.style;qo.opacity=io(qk[1][4].toString());}var qp=qk[1][5],qq=qp?l4(qp[1]):S,qr=qq.toString();if(caml_notequal(qj.style.backgroundColor,qr)){var qt=qj.style,qs=caml_equal(qr,R.toString())?Q.toString():qr;qt.backgroundColor=qs;}if(caml_string_notequal(qk[1][7],ql[1][7]))qj.id=qk[1][7].toString();if(caml_string_notequal(qk[1][6],ql[1][6]))if(caml_string_notequal(ql[1][6],P))qj.lastChild.href=qk[1][6].toString();else o2(qj,qk[1][6]);var qu=qk[1][8];if(qu){var qv=qu[1];if(iq(qj.oakHoverHandler))qj.oakHoverHandler=io(qv);else o0(qj,qv);}else{ir(qj.hoverOverId,i6);ir(qj.hoverOutId,i6);}var qw=qk[1][9];if(qw){var qx=qw[1],qy=iq(qj.oakClickHandler)?qj.oakClickHandler=io(qx):o1(qj,qx);}else var qy=ir(qj.clickId,i6);return qy;}var qz=ql[2],qA=qk[2];if(typeof qz==="number")var qE=typeof qA==="number"?1:0;else switch(qz[0]){case 1:if(typeof qA==="number"||!(1===qA[0]))var qE=0;else{var qB=qA[2],qC=qj.firstChild,qD=qA[1];dW(qe,qC,qz[2],qB);o3(qD,qB,qC);var qE=1;}break;case 2:if(typeof qA==="number"||!(2===qA[0]))var qE=0;else{var qF=qA[2],qG=qA[1],qI=qz[2];switch(qz[1]){case 1:var qH=1===qG?1:0;break;case 2:var qH=2===qG?1:0;break;case 3:var qH=3===qG?1:0;break;case 4:var qH=4===qG?1:0;break;case 5:var qH=5<=qG?1:0;break;default:var qH=0===qG?1:0;}if(qH){var qJ=qj.childNodes;if(qF.length-1!==qJ.length)qK(qj,qk);else{var qL=o4(qG),qM=qF.length-1,qN=qM-1|0,qO=o5(qG);for(;;){if(0<=qN){var qR=caml_array_get(qF,qN),qQ=caml_array_get(qI,qN),qP=qO?(qM-qN|0)-1|0:qN;dW(qe,qJ.item(qP),qQ,qR);cY(qL,qJ.item(qN));var qS=qN-1|0,qN=qS;continue;}break;}}}else qK(qj,qk);var qE=1;}break;case 3:if(typeof qA==="number"||!(3===qA[0]))var qE=0;else{var qT=qA[1];if(caml_string_notequal(qz[1],qT))qj.innerHTML=qT.toString();var qE=1;}break;case 4:var qE=0;break;default:if(typeof qA==="number"||!(0===qA[0]))var qE=0;else{var qU=qA[4],qV=qA[1],qW=qz[4];if(typeof qV==="number"&&0===qV){if(caml_string_notequal(qW,qU))oJ(qj,aS,qU.toString());var qX=1;}else var qX=0;if(!qX){var qY=caml_notequal(qk[2],ql[2])?0:qk[1][2]!==ql[1][2]?0:qk[1][3]!==ql[1][3]?0:1;if(!qY)qK(qj,qk);}var qE=1;}}if(qE)return 0;throw [0,c,aR];});function rd(q2,qZ){var q0=qZ[2],q1=qZ[1];if(0<q0[2]&&q2[2]==0){var q3=[0,q2[1],q2[2],q2[3],9,q2[5]],q4=1;}else var q4=0;if(!q4)var q3=q2;var q5=0<q3[2]?[0,q3[1],q3[2],q3[3],q3[4]-q1/3,q3[5]]:q3,q6=q0[1],q7=0<=q6?0<q6?k:q5[5]:l,q8=q5[4],q9=0<q5[2]?q5[3]:4*q6,q_=q5[2]+q1*q8,q$=0,rb=q5[1],ra=caml_greaterequal(q$,q_)?q$:q_;return [0,rb+q1*q9,ra,q9,q8,q7];}var rc=d[2],re=d[1];function rC(rg){var rf=20,rh=cY(m7,[0,rg[1],rg[2]-25-rf]),ri=2*rf,rj=50,rk=2*mW/rj,rp=cY(m6,l3(0,255,0,0)),rn=ri/2,rm=ri/2;function ro(rl){return [0,rn*Math.cos(rk*rl),rm*Math.sin(rk*rl)];}if(0===rj)var rq=[0];else{var rr=caml_make_vect(rj,ro(0)),rs=1,rt=rj-1|0;if(!(rt<rs)){var ru=rs;for(;;){rr[ru+1]=ro(ru);var rv=ru+1|0;if(rt!==ru){var ru=rv;continue;}break;}}var rq=rr;}var rw=j4(j4(rq,rp),rh),rx=cY(m7,[0,re/2,rc-12.5]),ry=cY(m6,l3(0,0,255,0)),rz=j4(j4(mR(re,25),ry),rx),rA=cY(m7,[0,re/2,rc/2]),rB=cY(m6,l3(0,174,238,238));return m2([3,aZ,[0,j4(j4(mR(re,rc),rB),rA),rz,rw]]);}var rE=j4(j4(i7.getElementById(j.toString()),ip),jv);function rI(rD){return rD/20;}var rH=1e3/30;function rK(rG,rF){return rF-rG;}var rJ=jS(0),rL=jS(0);function rQ(rP){var rN=setInterval(caml_js_wrap_callback(function(rM){return kl([0,rJ,rL],new iv().valueOf());}),rH);return function(rO){return clearInterval(rN);};}var rT=0,rS=jS(0),rU=lD(lS([0,[0,rQ,function(rR){return 0;},rJ,rS,rL,rT]],rK),rI),rV=jS(0),rW=jS(0);function rY(rX){return 0;}var r0=lg(mG[1],rY),r1=[0,lg(rU,function(rZ){return kl([0,rV,rW],[0,rZ,mG[2][1]]);}),[0,rU]],r2=[0,[0,r0,[0,mG[1]]],r1],r6=[1,[0,0,rV,jS(0),rW,r2]];function r4(r3){return [0,r3[1],rc-r3[2],r3[3],r3[4],r3[5]];}function r7(r5){return lT(r5,r4);}var r8=j4(lV(r6,i,rd),r7);j2(h,lU(r8));function r$(r9){return 0;}var r_=lT(r8,rC),sa=i_(i7,bu);if(1-(sa.getContext==ie?1:0)){var sb=sa.style;sb.width=ep(ic,x,re).toString();var sc=sa.style;sc.height=ep(ic,w,rc).toString();sa.style.display=v.toString();sa.style.position=u.toString();sa.width=re;sa.height=rc;var sd=sa.getContext(i$);i1(rE,sa);n1(sd,r_[2][1]);j3(y);var sg=function(se){return n1(sd,se);},sf=lU(r_),sh=lg(sf,sg);kH(sh,sf);j4(function(si){return kZ(sh,sf);},r$);cg(0);return;}throw [0,ja];}());
