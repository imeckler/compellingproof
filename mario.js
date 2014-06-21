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
(function(){function h_(sx,sy,sz,sA,sB,sC,sD){return sx.length==6?sx(sy,sz,sA,sB,sC,sD):caml_call_gen(sx,[sy,sz,sA,sB,sC,sD]);}function l1(sr,ss,st,su,sv,sw){return sr.length==5?sr(ss,st,su,sv,sw):caml_call_gen(sr,[ss,st,su,sv,sw]);}function dW(sn,so,sp,sq){return sn.length==3?sn(so,sp,sq):caml_call_gen(sn,[so,sp,sq]);}function ep(sk,sl,sm){return sk.length==2?sk(sl,sm):caml_call_gen(sk,[sl,sm]);}function cY(si,sj){return si.length==1?si(sj):caml_call_gen(si,[sj]);}var a=[0,new MlString("Failure")],b=[0,new MlString("Invalid_argument")],c=[0,new MlString("Match_failure")],d=[0,600,400];caml_register_global(6,[0,new MlString("Not_found")]);caml_register_global(5,[0,new MlString("Division_by_zero")]);caml_register_global(3,b);caml_register_global(2,a);var b4=[0,new MlString("Assert_failure")],b3=new MlString("%d"),b2=new MlString("true"),b1=new MlString("false"),b0=new MlString("Pervasives.do_at_exit"),bZ=new MlString("\\b"),bY=new MlString("\\t"),bX=new MlString("\\n"),bW=new MlString("\\r"),bV=new MlString("\\\\"),bU=new MlString("\\'"),bT=new MlString("String.blit"),bS=new MlString("String.sub"),bR=new MlString("Buffer.add: cannot grow buffer"),bQ=new MlString(""),bP=new MlString(""),bO=new MlString("%.12g"),bN=new MlString("\""),bM=new MlString("\""),bL=new MlString("'"),bK=new MlString("'"),bJ=new MlString("nan"),bI=new MlString("neg_infinity"),bH=new MlString("infinity"),bG=new MlString("."),bF=new MlString("printf: bad positional specification (0)."),bE=new MlString("%_"),bD=[0,new MlString("printf.ml"),143,8],bC=new MlString("'"),bB=new MlString("Printf: premature end of format string '"),bA=new MlString("'"),bz=new MlString(" in format string '"),by=new MlString(", at char number "),bx=new MlString("Printf: bad conversion %"),bw=new MlString("Sformat.index_of_int: negative argument "),bv=new MlString("on"),bu=new MlString("canvas"),bt=new MlString("img"),bs=new MlString("2d"),br=new MlString("Dom_html.Canvas_not_available"),bq=new MlString("Option.value_exn: None"),bp=new MlString("(function(a,f){var len=a.length;for(var i = 0; i < len; ++i){f(a[i]);}})"),bo=new MlString("iter"),bn=new MlString("iter"),bm=new MlString("(function(t, x0, f){for(var k in t){if(t.hasOwnProperty(k)){x0=f(x0,parseInt(k),t[k]);}} return x0;})"),bl=new MlString("(function(t, f){for(var k in t){if(t.hasOwnProperty(k)){f(parseInt(k),t[k]);}}})"),bk=new MlString("(function(t, x0, f){for(var k in t){if(t.hasOwnProperty(k)){x0=f(k,t[k],x0);}} return x0;})"),bj=new MlString("(function(t,f){for(var k in t){if(t.hasOwnProperty(k)){f(k,t[k]);}}})"),bi=new MlString("(function(x,y){return x % y;})"),bh=new MlString("Stream.turn_on_derived: Listener was not off or passive"),bg=new MlString("Stream.turn_off_derived: Listener was not on"),bf=new MlString("Stream.Prim.turn_off: Listener was not on or passive"),be=new MlString("Stream.Prim.turn_on: Listener was not off or passive"),bd=new MlString("rgba(%d,%d,%d,%f)"),bc=new MlString("pageY"),bb=new MlString("pageX"),ba=new MlString("mousemove"),a$=new MlString("which"),a_=new MlString("keyup"),a9=new MlString("keydown"),a8=new MlString("body"),a7=new MlString("key_stream"),a6=new MlString("%dpx"),a5=new MlString("%f%%"),a4=new MlString("left"),a3=new MlString("right"),a2=new MlString("center"),a1=new MlString("justify"),a0=new MlString("initial"),aZ=new MlString("inherit"),aY=[0,[0,0,0],[0,1,0,0,1]],aX=new MlString("\""),aW=new MlString("'"),aV=new MlString("<"),aU=new MlString(">"),aT=new MlString("\n"),aS=new MlString("a"),aR=new MlString("src"),aQ=[0,new MlString("src/oak_graphics.ml"),621,17],aP=[0,new MlString("src/oak_graphics.ml"),599,38],aO=new MlString("div"),aN=new MlString("div"),aM=new MlString("relative"),aL=new MlString("hidden"),aK=new MlString("div"),aJ=new MlString("none"),aI=new MlString("left"),aH=new MlString("absolute"),aG=new MlString("translate%c(%dpx) "),aF=new MlString(""),aE=new MlString(""),aD=new MlString("absolute"),aC=new MlString("auto"),aB=new MlString("top"),aA=new MlString("bottom"),az=new MlString("left"),ay=new MlString("right"),ax=new MlString(""),aw=new MlString("transform"),av=new MlString("msTransform"),au=new MlString("MozTransform"),at=new MlString("webkitTransform"),as=new MlString("OTransform"),ar=new MlString("div"),aq=new MlString("visible"),ap=new MlString("auto"),ao=new MlString("%dpx"),an=new MlString("%dpx"),am=new MlString("%dpx"),al=new MlString("%dpx"),ak=new MlString("div"),aj=new MlString("hidden"),ai=new MlString("img"),ah=new MlString("cover"),ag=new MlString("BackgroundSize"),af=new MlString("div"),ae=new MlString("url(%s) no-repeat center"),ad=new MlString("webkit"),ac=new MlString("Moz"),ab=new MlString("O"),aa=new MlString("cover"),$=new MlString("backgroundSize"),_=new MlString("div"),Z=new MlString("url(%s)"),Y=new MlString("block"),X=new MlString("%dpx"),W=new MlString("%dpx"),V=new MlString(""),U=new MlString(""),T=new MlString("%dpx"),S=new MlString("%dpx"),R=new MlString(""),Q=new MlString(""),P=new MlString("transparent"),O=new MlString(""),N=new MlString("a"),M=new MlString("100%"),L=new MlString("100%"),K=new MlString("0"),J=new MlString("0"),I=new MlString("block"),H=new MlString("absolute"),G=new MlString("auto"),F=new MlString("relative"),E=new MlString("auto"),D=new MlString("click"),C=new MlString("auto"),B=new MlString("mouseover"),A=new MlString("mouseout"),z=new MlString("0"),y=new MlString("0"),x=new MlString("%dpx"),w=new MlString("%dpx"),v=new MlString("block"),u=new MlString("absolute"),t=new MlString("repeat"),s=new MlString("butt"),r=new MlString("round"),q=new MlString("square"),p=new MlString("round"),o=new MlString("bevel"),n=new MlString("miter"),m=new MlString(""),l=new MlString("left"),k=new MlString("right"),j=new MlString("content"),i=[0,0,0,0,0,new MlString("right")],h=new MlString("mario_changes");function g(e){throw [0,a,e];}function b5(f){throw [0,b,f];}function ce(b6,b8){var b7=b6.getLen(),b9=b8.getLen(),b_=caml_create_string(b7+b9|0);caml_blit_string(b6,0,b_,0,b7);caml_blit_string(b8,0,b_,b7,b9);return b_;}function cf(b$){return caml_format_int(b3,b$);}function cg(cd){var ca=caml_ml_out_channels_list(0);for(;;){if(ca){var cb=ca[2];try {}catch(cc){}var ca=cb;continue;}return 0;}}caml_register_named_value(b0,cg);function ct(ch,cj){var ci=caml_create_string(ch);caml_fill_string(ci,0,ch,cj);return ci;}function cu(cm,ck,cl){if(0<=ck&&0<=cl&&!((cm.getLen()-cl|0)<ck)){var cn=caml_create_string(cl);caml_blit_string(cm,ck,cn,0,cl);return cn;}return b5(bS);}function cv(cq,cp,cs,cr,co){if(0<=co&&0<=cp&&!((cq.getLen()-co|0)<cp)&&0<=cr&&!((cs.getLen()-co|0)<cr))return caml_blit_string(cq,cp,cs,cr,co);return b5(bT);}var cw=caml_sys_const_word_size(0),cx=caml_mul(cw/8|0,(1<<(cw-10|0))-1|0)-1|0;function cP(cy){var cz=1<=cy?cy:1,cA=cx<cz?cx:cz,cB=caml_create_string(cA);return [0,cB,0,cA,cB];}function cQ(cC){return cu(cC[1],0,cC[2]);}function cJ(cD,cF){var cE=[0,cD[3]];for(;;){if(cE[1]<(cD[2]+cF|0)){cE[1]=2*cE[1]|0;continue;}if(cx<cE[1])if((cD[2]+cF|0)<=cx)cE[1]=cx;else g(bR);var cG=caml_create_string(cE[1]);cv(cD[1],0,cG,0,cD[2]);cD[1]=cG;cD[3]=cE[1];return 0;}}function cR(cH,cK){var cI=cH[2];if(cH[3]<=cI)cJ(cH,1);cH[1].safeSet(cI,cK);cH[2]=cI+1|0;return 0;}function cS(cN,cL){var cM=cL.getLen(),cO=cN[2]+cM|0;if(cN[3]<cO)cJ(cN,cM);cv(cL,0,cN[1],cN[2],cM);cN[2]=cO;return 0;}function cW(cT){return 0<=cT?cT:g(ce(bw,cf(cT)));}function cX(cU,cV){return cW(cU+cV|0);}var cZ=cY(cX,1);function c6(c0){return cu(c0,0,c0.getLen());}function c8(c1,c2,c4){var c3=ce(bz,ce(c1,bA)),c5=ce(by,ce(cf(c2),c3));return b5(ce(bx,ce(ct(1,c4),c5)));}function d2(c7,c_,c9){return c8(c6(c7),c_,c9);}function d3(c$){return b5(ce(bB,ce(c6(c$),bC)));}function dy(da,di,dk,dm){function dh(db){if((da.safeGet(db)-48|0)<0||9<(da.safeGet(db)-48|0))return db;var dc=db+1|0;for(;;){var dd=da.safeGet(dc);if(48<=dd){if(!(58<=dd)){var df=dc+1|0,dc=df;continue;}var de=0;}else if(36===dd){var dg=dc+1|0,de=1;}else var de=0;if(!de)var dg=db;return dg;}}var dj=dh(di+1|0),dl=cP((dk-dj|0)+10|0);cR(dl,37);var dn=dm,dp=0;for(;;){if(dn){var dq=dn[2],dr=[0,dn[1],dp],dn=dq,dp=dr;continue;}var ds=dj,dt=dp;for(;;){if(ds<=dk){var du=da.safeGet(ds);if(42===du){if(dt){var dv=dt[2];cS(dl,cf(dt[1]));var dw=dh(ds+1|0),ds=dw,dt=dv;continue;}throw [0,b4,bD];}cR(dl,du);var dx=ds+1|0,ds=dx;continue;}return cQ(dl);}}}function fs(dE,dC,dB,dA,dz){var dD=dy(dC,dB,dA,dz);if(78!==dE&&110!==dE)return dD;dD.safeSet(dD.getLen()-1|0,117);return dD;}function d4(dL,dV,d0,dF,dZ){var dG=dF.getLen();function dX(dH,dU){var dI=40===dH?41:125;function dT(dJ){var dK=dJ;for(;;){if(dG<=dK)return cY(dL,dF);if(37===dF.safeGet(dK)){var dM=dK+1|0;if(dG<=dM)var dN=cY(dL,dF);else{var dO=dF.safeGet(dM),dP=dO-40|0;if(dP<0||1<dP){var dQ=dP-83|0;if(dQ<0||2<dQ)var dR=1;else switch(dQ){case 1:var dR=1;break;case 2:var dS=1,dR=0;break;default:var dS=0,dR=0;}if(dR){var dN=dT(dM+1|0),dS=2;}}else var dS=0===dP?0:1;switch(dS){case 1:var dN=dO===dI?dM+1|0:dW(dV,dF,dU,dO);break;case 2:break;default:var dN=dT(dX(dO,dM+1|0)+1|0);}}return dN;}var dY=dK+1|0,dK=dY;continue;}}return dT(dU);}return dX(d0,dZ);}function es(d1){return dW(d4,d3,d2,d1);}function eI(d5,ee,eo){var d6=d5.getLen()-1|0;function eq(d7){var d8=d7;a:for(;;){if(d8<d6){if(37===d5.safeGet(d8)){var d9=0,d_=d8+1|0;for(;;){if(d6<d_)var d$=d3(d5);else{var ea=d5.safeGet(d_);if(58<=ea){if(95===ea){var ec=d_+1|0,eb=1,d9=eb,d_=ec;continue;}}else if(32<=ea)switch(ea-32|0){case 1:case 2:case 4:case 5:case 6:case 7:case 8:case 9:case 12:case 15:break;case 0:case 3:case 11:case 13:var ed=d_+1|0,d_=ed;continue;case 10:var ef=dW(ee,d9,d_,105),d_=ef;continue;default:var eg=d_+1|0,d_=eg;continue;}var eh=d_;c:for(;;){if(d6<eh)var ei=d3(d5);else{var ej=d5.safeGet(eh);if(126<=ej)var ek=0;else switch(ej){case 78:case 88:case 100:case 105:case 111:case 117:case 120:var ei=dW(ee,d9,eh,105),ek=1;break;case 69:case 70:case 71:case 101:case 102:case 103:var ei=dW(ee,d9,eh,102),ek=1;break;case 33:case 37:case 44:case 64:var ei=eh+1|0,ek=1;break;case 83:case 91:case 115:var ei=dW(ee,d9,eh,115),ek=1;break;case 97:case 114:case 116:var ei=dW(ee,d9,eh,ej),ek=1;break;case 76:case 108:case 110:var el=eh+1|0;if(d6<el){var ei=dW(ee,d9,eh,105),ek=1;}else{var em=d5.safeGet(el)-88|0;if(em<0||32<em)var en=1;else switch(em){case 0:case 12:case 17:case 23:case 29:case 32:var ei=ep(eo,dW(ee,d9,eh,ej),105),ek=1,en=0;break;default:var en=1;}if(en){var ei=dW(ee,d9,eh,105),ek=1;}}break;case 67:case 99:var ei=dW(ee,d9,eh,99),ek=1;break;case 66:case 98:var ei=dW(ee,d9,eh,66),ek=1;break;case 41:case 125:var ei=dW(ee,d9,eh,ej),ek=1;break;case 40:var ei=eq(dW(ee,d9,eh,ej)),ek=1;break;case 123:var er=dW(ee,d9,eh,ej),et=dW(es,ej,d5,er),eu=er;for(;;){if(eu<(et-2|0)){var ev=ep(eo,eu,d5.safeGet(eu)),eu=ev;continue;}var ew=et-1|0,eh=ew;continue c;}default:var ek=0;}if(!ek)var ei=d2(d5,eh,ej);}var d$=ei;break;}}var d8=d$;continue a;}}var ex=d8+1|0,d8=ex;continue;}return d8;}}eq(0);return 0;}function gH(eJ){var ey=[0,0,0,0];function eH(eD,eE,ez){var eA=41!==ez?1:0,eB=eA?125!==ez?1:0:eA;if(eB){var eC=97===ez?2:1;if(114===ez)ey[3]=ey[3]+1|0;if(eD)ey[2]=ey[2]+eC|0;else ey[1]=ey[1]+eC|0;}return eE+1|0;}eI(eJ,eH,function(eF,eG){return eF+1|0;});return ey[1];}function fo(eK,eN,eL){var eM=eK.safeGet(eL);if((eM-48|0)<0||9<(eM-48|0))return ep(eN,0,eL);var eO=eM-48|0,eP=eL+1|0;for(;;){var eQ=eK.safeGet(eP);if(48<=eQ){if(!(58<=eQ)){var eT=eP+1|0,eS=(10*eO|0)+(eQ-48|0)|0,eO=eS,eP=eT;continue;}var eR=0;}else if(36===eQ)if(0===eO){var eU=g(bF),eR=1;}else{var eU=ep(eN,[0,cW(eO-1|0)],eP+1|0),eR=1;}else var eR=0;if(!eR)var eU=ep(eN,0,eL);return eU;}}function fj(eV,eW){return eV?eW:cY(cZ,eW);}function e_(eX,eY){return eX?eX[1]:eY;}function h9(g2,e0,hc,e3,gM,hi,eZ){var e1=cY(e0,eZ);function g3(e2){return ep(e3,e1,e2);}function gL(e8,hh,e4,fb){var e7=e4.getLen();function gI(g$,e5){var e6=e5;for(;;){if(e7<=e6)return cY(e8,e1);var e9=e4.safeGet(e6);if(37===e9){var ff=function(fa,e$){return caml_array_get(fb,e_(fa,e$));},fl=function(fn,fg,fi,fc){var fd=fc;for(;;){var fe=e4.safeGet(fd)-32|0;if(!(fe<0||25<fe))switch(fe){case 1:case 2:case 4:case 5:case 6:case 7:case 8:case 9:case 12:case 15:break;case 10:return fo(e4,function(fh,fm){var fk=[0,ff(fh,fg),fi];return fl(fn,fj(fh,fg),fk,fm);},fd+1|0);default:var fp=fd+1|0,fd=fp;continue;}var fq=e4.safeGet(fd);if(124<=fq)var fr=0;else switch(fq){case 78:case 88:case 100:case 105:case 111:case 117:case 120:var ft=ff(fn,fg),fu=caml_format_int(fs(fq,e4,e6,fd,fi),ft),fw=fv(fj(fn,fg),fu,fd+1|0),fr=1;break;case 69:case 71:case 101:case 102:case 103:var fx=ff(fn,fg),fy=caml_format_float(dy(e4,e6,fd,fi),fx),fw=fv(fj(fn,fg),fy,fd+1|0),fr=1;break;case 76:case 108:case 110:var fz=e4.safeGet(fd+1|0)-88|0;if(fz<0||32<fz)var fA=1;else switch(fz){case 0:case 12:case 17:case 23:case 29:case 32:var fB=fd+1|0,fC=fq-108|0;if(fC<0||2<fC)var fD=0;else{switch(fC){case 1:var fD=0,fE=0;break;case 2:var fF=ff(fn,fg),fG=caml_format_int(dy(e4,e6,fB,fi),fF),fE=1;break;default:var fH=ff(fn,fg),fG=caml_format_int(dy(e4,e6,fB,fi),fH),fE=1;}if(fE){var fI=fG,fD=1;}}if(!fD){var fJ=ff(fn,fg),fI=caml_int64_format(dy(e4,e6,fB,fi),fJ);}var fw=fv(fj(fn,fg),fI,fB+1|0),fr=1,fA=0;break;default:var fA=1;}if(fA){var fK=ff(fn,fg),fL=caml_format_int(fs(110,e4,e6,fd,fi),fK),fw=fv(fj(fn,fg),fL,fd+1|0),fr=1;}break;case 37:case 64:var fw=fv(fg,ct(1,fq),fd+1|0),fr=1;break;case 83:case 115:var fM=ff(fn,fg);if(115===fq)var fN=fM;else{var fO=[0,0],fP=0,fQ=fM.getLen()-1|0;if(!(fQ<fP)){var fR=fP;for(;;){var fS=fM.safeGet(fR),fT=14<=fS?34===fS?1:92===fS?1:0:11<=fS?13<=fS?1:0:8<=fS?1:0,fU=fT?2:caml_is_printable(fS)?1:4;fO[1]=fO[1]+fU|0;var fV=fR+1|0;if(fQ!==fR){var fR=fV;continue;}break;}}if(fO[1]===fM.getLen())var fW=fM;else{var fX=caml_create_string(fO[1]);fO[1]=0;var fY=0,fZ=fM.getLen()-1|0;if(!(fZ<fY)){var f0=fY;for(;;){var f1=fM.safeGet(f0),f2=f1-34|0;if(f2<0||58<f2)if(-20<=f2)var f3=1;else{switch(f2+34|0){case 8:fX.safeSet(fO[1],92);fO[1]+=1;fX.safeSet(fO[1],98);var f4=1;break;case 9:fX.safeSet(fO[1],92);fO[1]+=1;fX.safeSet(fO[1],116);var f4=1;break;case 10:fX.safeSet(fO[1],92);fO[1]+=1;fX.safeSet(fO[1],110);var f4=1;break;case 13:fX.safeSet(fO[1],92);fO[1]+=1;fX.safeSet(fO[1],114);var f4=1;break;default:var f3=1,f4=0;}if(f4)var f3=0;}else var f3=(f2-1|0)<0||56<(f2-1|0)?(fX.safeSet(fO[1],92),fO[1]+=1,fX.safeSet(fO[1],f1),0):1;if(f3)if(caml_is_printable(f1))fX.safeSet(fO[1],f1);else{fX.safeSet(fO[1],92);fO[1]+=1;fX.safeSet(fO[1],48+(f1/100|0)|0);fO[1]+=1;fX.safeSet(fO[1],48+((f1/10|0)%10|0)|0);fO[1]+=1;fX.safeSet(fO[1],48+(f1%10|0)|0);}fO[1]+=1;var f5=f0+1|0;if(fZ!==f0){var f0=f5;continue;}break;}}var fW=fX;}var fN=ce(bM,ce(fW,bN));}if(fd===(e6+1|0))var f6=fN;else{var f7=dy(e4,e6,fd,fi);try {var f8=0,f9=1;for(;;){if(f7.getLen()<=f9)var f_=[0,0,f8];else{var f$=f7.safeGet(f9);if(49<=f$)if(58<=f$)var ga=0;else{var f_=[0,caml_int_of_string(cu(f7,f9,(f7.getLen()-f9|0)-1|0)),f8],ga=1;}else{if(45===f$){var gc=f9+1|0,gb=1,f8=gb,f9=gc;continue;}var ga=0;}if(!ga){var gd=f9+1|0,f9=gd;continue;}}var ge=f_;break;}}catch(gf){if(gf[1]!==a)throw gf;var ge=c8(f7,0,115);}var gg=ge[1],gh=fN.getLen(),gi=0,gm=ge[2],gl=32;if(gg===gh&&0===gi){var gj=fN,gk=1;}else var gk=0;if(!gk)if(gg<=gh)var gj=cu(fN,gi,gh);else{var gn=ct(gg,gl);if(gm)cv(fN,gi,gn,0,gh);else cv(fN,gi,gn,gg-gh|0,gh);var gj=gn;}var f6=gj;}var fw=fv(fj(fn,fg),f6,fd+1|0),fr=1;break;case 67:case 99:var go=ff(fn,fg);if(99===fq)var gp=ct(1,go);else{if(39===go)var gq=bU;else if(92===go)var gq=bV;else{if(14<=go)var gr=0;else switch(go){case 8:var gq=bZ,gr=1;break;case 9:var gq=bY,gr=1;break;case 10:var gq=bX,gr=1;break;case 13:var gq=bW,gr=1;break;default:var gr=0;}if(!gr)if(caml_is_printable(go)){var gs=caml_create_string(1);gs.safeSet(0,go);var gq=gs;}else{var gt=caml_create_string(4);gt.safeSet(0,92);gt.safeSet(1,48+(go/100|0)|0);gt.safeSet(2,48+((go/10|0)%10|0)|0);gt.safeSet(3,48+(go%10|0)|0);var gq=gt;}}var gp=ce(bK,ce(gq,bL));}var fw=fv(fj(fn,fg),gp,fd+1|0),fr=1;break;case 66:case 98:var gv=fd+1|0,gu=ff(fn,fg)?b2:b1,fw=fv(fj(fn,fg),gu,gv),fr=1;break;case 40:case 123:var gw=ff(fn,fg),gx=dW(es,fq,e4,fd+1|0);if(123===fq){var gy=cP(gw.getLen()),gC=function(gA,gz){cR(gy,gz);return gA+1|0;};eI(gw,function(gB,gE,gD){if(gB)cS(gy,bE);else cR(gy,37);return gC(gE,gD);},gC);var gF=cQ(gy),fw=fv(fj(fn,fg),gF,gx),fr=1;}else{var gG=fj(fn,fg),gJ=cX(gH(gw),gG),fw=gL(function(gK){return gI(gJ,gx);},gG,gw,fb),fr=1;}break;case 33:cY(gM,e1);var fw=gI(fg,fd+1|0),fr=1;break;case 41:var fw=fv(fg,bQ,fd+1|0),fr=1;break;case 44:var fw=fv(fg,bP,fd+1|0),fr=1;break;case 70:var gN=ff(fn,fg);if(0===fi)var gO=bO;else{var gP=dy(e4,e6,fd,fi);if(70===fq)gP.safeSet(gP.getLen()-1|0,103);var gO=gP;}var gQ=caml_classify_float(gN);if(3===gQ)var gR=gN<0?bI:bH;else if(4<=gQ)var gR=bJ;else{var gS=caml_format_float(gO,gN),gT=0,gU=gS.getLen();for(;;){if(gU<=gT)var gV=ce(gS,bG);else{var gW=gS.safeGet(gT)-46|0,gX=gW<0||23<gW?55===gW?1:0:(gW-1|0)<0||21<(gW-1|0)?1:0;if(!gX){var gY=gT+1|0,gT=gY;continue;}var gV=gS;}var gR=gV;break;}}var fw=fv(fj(fn,fg),gR,fd+1|0),fr=1;break;case 91:var fw=d2(e4,fd,fq),fr=1;break;case 97:var gZ=ff(fn,fg),g0=cY(cZ,e_(fn,fg)),g1=ff(0,g0),g5=fd+1|0,g4=fj(fn,g0);if(g2)g3(ep(gZ,0,g1));else ep(gZ,e1,g1);var fw=gI(g4,g5),fr=1;break;case 114:var fw=d2(e4,fd,fq),fr=1;break;case 116:var g6=ff(fn,fg),g8=fd+1|0,g7=fj(fn,fg);if(g2)g3(cY(g6,0));else cY(g6,e1);var fw=gI(g7,g8),fr=1;break;default:var fr=0;}if(!fr)var fw=d2(e4,fd,fq);return fw;}},hb=e6+1|0,g_=0;return fo(e4,function(ha,g9){return fl(ha,g$,g_,g9);},hb);}ep(hc,e1,e9);var hd=e6+1|0,e6=hd;continue;}}function fv(hg,he,hf){g3(he);return gI(hg,hf);}return gI(hh,0);}var hj=ep(gL,hi,cW(0)),hk=gH(eZ);if(hk<0||6<hk){var hx=function(hl,hr){if(hk<=hl){var hm=caml_make_vect(hk,0),hp=function(hn,ho){return caml_array_set(hm,(hk-hn|0)-1|0,ho);},hq=0,hs=hr;for(;;){if(hs){var ht=hs[2],hu=hs[1];if(ht){hp(hq,hu);var hv=hq+1|0,hq=hv,hs=ht;continue;}hp(hq,hu);}return ep(hj,eZ,hm);}}return function(hw){return hx(hl+1|0,[0,hw,hr]);};},hy=hx(0,0);}else switch(hk){case 1:var hy=function(hA){var hz=caml_make_vect(1,0);caml_array_set(hz,0,hA);return ep(hj,eZ,hz);};break;case 2:var hy=function(hC,hD){var hB=caml_make_vect(2,0);caml_array_set(hB,0,hC);caml_array_set(hB,1,hD);return ep(hj,eZ,hB);};break;case 3:var hy=function(hF,hG,hH){var hE=caml_make_vect(3,0);caml_array_set(hE,0,hF);caml_array_set(hE,1,hG);caml_array_set(hE,2,hH);return ep(hj,eZ,hE);};break;case 4:var hy=function(hJ,hK,hL,hM){var hI=caml_make_vect(4,0);caml_array_set(hI,0,hJ);caml_array_set(hI,1,hK);caml_array_set(hI,2,hL);caml_array_set(hI,3,hM);return ep(hj,eZ,hI);};break;case 5:var hy=function(hO,hP,hQ,hR,hS){var hN=caml_make_vect(5,0);caml_array_set(hN,0,hO);caml_array_set(hN,1,hP);caml_array_set(hN,2,hQ);caml_array_set(hN,3,hR);caml_array_set(hN,4,hS);return ep(hj,eZ,hN);};break;case 6:var hy=function(hU,hV,hW,hX,hY,hZ){var hT=caml_make_vect(6,0);caml_array_set(hT,0,hU);caml_array_set(hT,1,hV);caml_array_set(hT,2,hW);caml_array_set(hT,3,hX);caml_array_set(hT,4,hY);caml_array_set(hT,5,hZ);return ep(hj,eZ,hT);};break;default:var hy=ep(hj,eZ,[0]);}return hy;}function h8(h0){return cP(2*h0.getLen()|0);}function h5(h3,h1){var h2=cQ(h1);h1[2]=0;return cY(h3,h2);}function ib(h4){var h7=cY(h5,h4);return h_(h9,1,h8,cR,cS,function(h6){return 0;},h7);}function ic(ia){return ep(ib,function(h$){return h$;},ia);}var id=[0,0];32===cw;var ie=null,ig=undefined;function ip(ih){var ii=ih==ie?0:[0,ih];return ii;}function io(ij){return ij;}function iq(ik){return ik!==ig?1:0;}function ir(il,im){return il!==ig?cY(im,il):0;}var is=false,it=Array,iw=RegExp,iv=Date;function ix(iu){return iu instanceof it?0:[0,new MlWrappedString(iu.toString())];}id[1]=[0,ix,id[1]];function iz(iy){return iy;}function i1(iA,iB){iA.appendChild(iB);return 0;}function iG(iC){return event;}function i2(iE){return iz(caml_js_wrap_callback(function(iD){if(iD){var iF=cY(iE,iD);if(!(iF|0))iD.preventDefault();return iF;}var iH=iG(0),iI=cY(iE,iH);if(!(iI|0))iH.returnValue=iI;return iI;}));}function i3(iJ,iK,iL){return iJ.call(iK,iL);}function i4(iM){return iM.toString();}function i5(iN,iO,iR,iY){if(iN.addEventListener===ig){var iP=bv.toString().concat(iO),iW=function(iQ){var iV=[0,iR,iQ,[0]];return cY(function(iU,iT,iS){return caml_js_call(iU,iT,iS);},iV);};iN.attachEvent(iP,iW);return function(iX){return iN.detachEvent(iP,iW);};}iN.addEventListener(iO,iR,iY);return function(iZ){return iN.removeEventListener(iO,iR,iY);};}function i6(i0){return cY(i0,0);}var i7=this.document,i$=bs.toString();function i_(i9,i8){return i9.createElement(i8.toString());}var ja=[0,br];this.HTMLElement===ig;function je(jb,jc){return jb?cY(jc,jb[1]):0;}function jv(jd){return jd?jd[1]:g(bq);}function ju(jg,jj){var jf=0,jh=jg.length-1-1|0;if(!(jh<jf)){var ji=jf;for(;;){cY(jj,jg[ji+1]);var jk=ji+1|0;if(jh!==ji){var ji=jk;continue;}break;}}return 0;}function jw(jl,jp,jr){var jm=jl?jl[1]:function(jo,jn){return caml_equal(jo,jn);},jq=jp.length-1-1|0;for(;;){if(0<=jq){if(!ep(jm,caml_array_get(jp,jq),jr)){var jt=jq-1|0,jq=jt;continue;}var js=1;}else var js=0;return js;}}var jx=caml_js_eval_string(bp);({}[bo.toString()]=jx);var jy=caml_js_eval_string(bm),jH={"iter":caml_js_eval_string(bl),"fold":jy};function jS(jz){return {};}function jT(jA,jB,jC){return jA[jB]=jC;}function jU(jD,jE){return delete jD[jE];}function jV(jF,jG){return jF.hasOwnProperty(jG)|0?[0,jF[jG]]:0;}function jQ(jM,jK){var jL=jH[bn.toString()];return jL(jM,caml_js_wrap_callback(function(jJ,jI){return ep(jK,jJ,jI);}));}function jW(jR){var jN=[0,0];jQ(jR,function(jO,jP){jN[1]+=1;return 0;});return jN[1];}var jX=caml_js_eval_string(bk);({"iter":caml_js_eval_string(bj),"fold":jX});caml_js_eval_string(bi);function j2(jY,jZ){return window[jY.toString()]=jZ;}function j3(j0,j1){return cY(j1,j0);}function j8(j7,j4){return jQ(j7,function(j6,j5){return cY(j5,j4);});}function kk(j$,j9){return ju(j$,function(j_){return j8(j_,j9);});}function k7(kd,ka,kc){var kb=ka[6];ka[6]=ka[6]+1|0;jT(kd,kb,kc);return kb;}function lQ(ke,ks){var kf=ke?ke[1]:function(kg,kh){return 0;},ki=jS(0),kj=jS(0);function km(kl){return kk([0,ki,kj],kl);}var kq=0,kp=jS(0);function kr(kn){return 0;}return [0,[0,function(ko){return cY(kf,km);},kr,ki,kp,kj,kq]];}function kG(kw,kt){{if(0===kt[0]){var ku=kt[1],ky=function(kx,kv){jT(ku[3],kw,kv);jU(kx,kw);return 1===jW(ku[3])?(ku[2]=cY(ku[1],0),0):0;},kz=jV(ku[4],kw);if(kz)var kA=ky(ku[4],kz[1]);else{var kB=jV(ku[5],kw),kA=kB?ky(ku[5],kB[1]):g(be);}return kA;}var kC=kt[1],kI=function(kE,kD){jT(kC[2],kw,kD);jU(kE,kw);if(1===jW(kC[2])){var kH=function(kF){return kG(kF[1],kF[2][1]);};return ju(kC[5],kH);}return 0;},kJ=jV(kC[3],kw);if(kJ)var kK=kI(kC[3],kJ[1]);else{var kL=jV(kC[4],kw),kK=kL?kI(kC[4],kL[1]):g(bh);}return kK;}}function kY(kP,kM){{if(0===kM[0]){var kN=kM[1],kR=function(kQ,kO){jT(kN[4],kP,kO);return jU(kQ,kP);},kS=jV(kN[3],kP);if(kS){kR(kN[3],kS[1]);var kT=0===jW(kN[3])?cY(kN[2],0):0;}else{var kU=jV(kN[5],kP),kT=kU?kR(kN[5],kU[1]):g(bf);}return kT;}var kV=kM[1],kW=jV(kV[2],kP);if(kW){jT(kV[3],kP,kW[1]);jU(kV[2],kP);if(0===jW(kV[2])){var kZ=function(kX){return kY(kX[1],kX[2][1]);},k0=ju(kV[5],kZ);}else var k0=0;}else var k0=g(bg);return k0;}}function k_(k4,k1,k3){var k2=k1[1];k1[1]=k1[1]+1|0;jT(k4,k2,k3);return k2;}function lf(k5,k8){{if(0===k5[0]){var k6=k5[1];return k7(k6[4],k6,k8);}var k9=k5[1];return k_(k9[3],k9,k8);}}function ll(lg,le){var k$=jS(0),la=jS(0);function ld(lb){j8(k$,lb);return j8(la,lb);}var lh=[0,[0,lf(lg,function(lc){return ep(le,ld,lc);}),[0,lg]]];return [1,[0,0,k$,jS(0),la,lh]];}function lC(lm,lj){return ll(lm,function(lk,li){return cY(lk,cY(lj,li));});}function lR(lt,lq){var ln=[0,0];return ll(lt,function(lr,lo){function ls(lp){return cY(lr,ep(lq,lp,lo));}je(ln[1],ls);ln[1]=[0,lo];return 0;});}function lG(lu,lx){function lw(lv){lu[1]=lv;return 0;}if(0===lx[0]){var ly=lx[1],lz=k7(ly[5],ly,lw);}else{var lA=lx[1],lz=k_(lA[4],lA,lw);}return [0,lx,lu,[0,lz]];}function lS(lB,lD){var lE=lB[2],lF=lC(lB[1],lD);return lG([0,cY(lD,lE[1])],lF);}function lT(lH){return lH[1];}function lU(lP,lI,lL){var lJ=[0,lI],lO=[0,lI];return lG(lO,ll(lP,function(lN,lK){var lM=ep(lL,lJ[1],lK);lJ[1]=lM;return cY(lN,lM);}));}function l2(lV,lZ,lY,lX){var lW=lV?lV[1]:1;return [0,lZ,lY,lX,lW];}function l3(l0){return l1(ic,bd,l0[1],l0[2],l0[3],l0[4]);}function l5(l4){return l4;}var l6=jQuery(a8.toString());function mj(mg,l9){function l8(l7){return [0,l7[1],caml_js_wrap_callback(l7[2])];}var l_=l9.length-1;if(0===l_)var l$=[0];else{var ma=caml_make_vect(l_,l8(l9[0+1])),mb=1,mc=l_-1|0;if(!(mc<mb)){var md=mb;for(;;){ma[md+1]=l8(l9[md+1]);var me=md+1|0;if(mc!==md){var md=me;continue;}break;}}var l$=ma;}ju(l$,function(mf){return mg.on(mf[1].toString(),mf[2]);});return function(mi){return ju(l$,function(mh){return mg.off(mh[1].toString(),mh[2]);});};}var mq=0,mr=lQ([0,function(mn){function mm(mk){return mk[a$.toString()];}var mp=[0,a_,function(ml){ml.preventDefault();return cY(mn,[0,19067,mm(ml)]);}];return mj(l6,[0,[0,a9,function(mo){mo.preventDefault();return cY(mn,[0,759637122,mm(mo)]);}],mp]);}],mq);j2(a7,mr);var ms=jS(0),mE=lU(mr,[0],function(mx,mt){if(759637122<=mt[1])jT(ms,mt[2],0);else jU(ms,mt[2]);var mu=new it();jQ(ms,function(mv,mw){return mu.push(mv);});return caml_js_to_array(mu);});function mA(my){return 0===my?0:1;}var mF=lS(mE,function(mz){var mB=mA(jw(0,mz,l5(40))),mC=mA(jw(0,mz,l5(38)))-mB|0,mD=mA(jw(0,mz,l5(37)));return [0,mA(jw(0,mz,l5(39)))-mD|0,mC];}),mI=0,mL=lQ([0,function(mH){return mj(l6,[0,[0,ba,function(mG){return cY(mH,[0,mG[bb.toString()],mG[bc.toString()]]);}]]);}],mI);lR(mL,function(mK,mJ){return [0,mJ[1]-mK[1]|0,mJ[2]-mK[2]|0];});function mQ(mM,mO){var mN=mM/2,mP=mO/2;return [0,[0,-mN,-mP],[0,-mN,mP],[0,mN,mP],[0,mN,-mP]];}var mV=4*Math.atan(1);function mU(mR){var mS=0===mR[0]?ep(ic,a6,mR[1]):ep(ic,a5,mR[1]);return mS.toString();}function mW(mT){return new iw(mT.toString());}mW(aX);mW(aW);mW(aV);mW(aU);mW(aT);function m4(mX){return Math.round(mX);}function m1(mY){return [0,0,1,0,0,1,mY];}function m5(mZ,m0){return m1([1,[1,[0,mZ]],m0]);}function m6(m2,m3){return [0,m3[1],m3[2],m3[3]+m2[1],m3[4]+m2[2],m3[5],m3[6]];}[0,0][1]+=1;function no(m9,m7,nd){if(1<m7.length-1){var m8=caml_array_get(m7,0);m9.moveTo(m8[1],m8[2]);var m_=1,m$=m7.length-1-1|0;if(!(m$<m_)){var na=m_;for(;;){var nb=caml_array_get(m7,na);m9.lineTo(nb[1],nb[2]);var nc=na+1|0;if(m$!==na){var na=nc;continue;}break;}}if(nd){var ne=caml_array_get(m7,0);return m9.lineTo(ne[1],ne[2]);}return 0;}return 0;}function nC(ng,nf,nq,np){ng.lineWidth=nf[2];var nh=nf[3],ni=416330352===nh?q:781662169<=nh?s:r;ng.lineCap=ni.toString();var nj=nf[4],nk=typeof nj==="number"?1006599246<=nj?p:o:n;ng.lineJoin=nk.toString();var nl=nf[4];if(typeof nl==="number"||!(256529610===nl[1]))var nm=0;else{var nn=nl[2],nm=1;}if(!nm)var nn=10;ng.miterLimit=nn;ng.strokeStyle=l3(nf[1]).toString();if(0===nf[5].length-1)no(ng,nq,np);else if(1<nq.length-1){var nr=caml_array_get(nq,nq.length-1-1|0);ng.moveTo(nr[1],nr[2]);g(m);}ng.scale(1,-1);return ng.stroke();}function n0(nz,ns){var nt=ns[6],nu=ns[5],nv=ns[4],nw=ns[3],nx=ns[2],ny=ns[1];nz.save();var nA=nw!=0?0:nv!=0?0:1;if(!nA)nz.translate(nw,nv);if(ny!=0)nz.rotate(ny);if(nx!=1)nz.scale(nx,nx);if(nu!=1)nz.globalAlpha=nz.globalAlpha*nu;nz.beginPath();switch(nt[0]){case 1:var nB=nt[1];if(0===nB[0])nC(nz,nB[1],nt[2],1);else{var nD=nB[1];no(nz,nt[2],0);switch(nD[0]){case 1:nz.fillStyle=nz.createPattern(nD[1],t.toString());break;case 2:var nE=nD[1];if(0===nE[0]){var nF=nE[2],nG=nE[1],nH=nE[3],nI=[0,nz.createLinearGradient(nG[1],-nG[2],nF[1],-nF[2]),nH];}else{var nJ=nE[3],nK=nE[1],nL=nE[5],nI=[0,nz.createRadialGradient(nK[1],-nK[2],nE[2],nJ[1],-nJ[2],nE[4]),nL];}var nM=nI[1],nP=nI[2];ju(nP,function(nN){var nO=nN[1];return nM.addColorStop(nO,l3(nN[2]).toString());});nz.fillStyle=nM;break;default:nz.fillStyle=l3(nD[1]).toString();}nz.scale(1,-1);nz.fill();}break;case 2:var nQ=nt[3],nR=nt[2],nS=nt[1],nV=nt[4],nU=nQ[2],nT=nQ[1];nz.scale(1,-1);nz.drawImage(nV,nT,nU,nS,nR,-nS/2,-nR/2,nS,nR);break;case 3:var nW=nt[1],nX=nW[2],nY=nW[1],nZ=nt[2];nz.transform(nX[1],nX[2],nX[3],nX[4],nY[1],nY[2]);ju(nZ,cY(n0,nz));break;default:nC(nz,nt[1],nt[2],0);}return nz.restore();}function on(n1){var n2=i7.createElement(n1.toString());n2.style.padding=z.toString();n2.style.margin=y.toString();return n2;}function oZ(n3,n4){n3.style.pointerEvents=C.toString();n3.oakHoverHandler=io(n4);n3.hoverTriggered=0;function n_(n6,n7){return i2(function(n5){if(!cY(n6,n5)){n3.hoverTriggered=n7;var n9=function(n8){i3(n8,0,n5);return 0;};ir(n3.oakHoverHandler,n9);n5.stopPropagation();}return is;});}var oa=1,od=n_(function(n$){return n3.hoverTriggered;},oa),oc=0,oe=n_(function(ob){return n3.contains(ob.toElement);},oc),of=i5({},i4(B),od,is),og=i5({},i4(A),oe,is);n3.hoverOverId=io(of);return n3.hoverOutId=io(og);}function o0(oh,oi){oh.style.pointerEvents=E.toString();oh.oakClickHandler=io(oi);var om=i2(function(oj){function ol(ok){i3(ok,0,oj);return 0;}ir(oh.oakClickHandler,ol);return oj.stopPropagation();});return oh.clickId=io(i5({},i4(D),om,is));}function o1(or,oq){var oo=on(N),op=oo.style;oo.href=oq.toString();op.width=M.toString();op.height=L.toString();op.top=K.toString();op.left=J.toString();op.display=I.toString();op.position=H.toString();op.pointerEvents=G.toString();or.style.position=F.toString();return i1(or,oo);}function oI(ot,os,ou){return ot[os.toString()]=ou;}function o2(ox,ov,oy){var ow=ov[1],oz=oy.style,oF=ow[3],oE=ow[2],oD=ox[4],oC=ox[3],oB=ox[2],oA=ox[1];oz.position=aD.toString();oz.margin=aC.toString();function oN(oG,oM,oJ,oK,oL,oH){return 80===oG?(oI(oz,oJ,mU(oH)),oz.removeProperty(oK.toString()),aE):90<=oG?dW(ic,aG,oM,(-oL|0)/2|0):(oI(oz,oK,mU(oH)),oz.removeProperty(oK.toString()),aF);}var oO=oN(oB,89,aA,aB,oF,oD),oP=ce(oN(oA,88,ay,az,oE,oC),oO);if(caml_string_notequal(oP,ax)){var oQ=oP.toString();oI(oz,aw,oQ);oI(oz,av,oQ);oI(oz,au,oQ);oI(oz,at,oQ);return oI(oz,as,oQ);}return 0;}function oX(oR){return oR;}function oV(oS){oS.style.position=aH.toString();return oS;}function oW(oT){oT.style.cssFloat=aI.toString();return oT;}function o3(oU){return 2<=oU?4<=oU?oV:oW:oX;}function o4(oY){switch(oY){case 0:case 2:case 4:return 1;default:return 0;}}var o5=[],o6=[],o7=[],o8=[];caml_update_dummy(o5,function(o_,pc){var o9=on(aK);if(5<=o_)o9.style.pointerEvents=aJ.toString();var pa=o3(o_);function pb(o$){return i1(o9,cY(pa,cY(o8,o$)));}if(o4(o_)){var pd=pc.length-1-1|0;for(;;){if(0<=pd){pb(caml_array_get(pc,pd));var pe=pd-1|0,pd=pe;continue;}break;}}else ju(pc,pb);return o9;});caml_update_dummy(o6,function(ph,pf){var pg=cY(o8,pf);o2(ph,pf,pg);var pi=on(aN);pi.style.position=aM.toString();pi.style.overflow=aL.toString();i1(pi,pg);return pi;});caml_update_dummy(o7,function(pj){var pk=pj[2],pl=pj[1];if(typeof pk==="number")return on(aO);else switch(pk[0]){case 0:var pm=pk[4],pn=pk[1],pG=pk[3],pD=pk[2],pF=pl[3],pC=pl[2];if(typeof pn==="number")switch(pn){case 1:var po=on(af),ps=function(pp){var pq=ah.toString(),pr=ce(pp,ag);return oI(po.style,pr,pq);},pt=po.style;pt.background=ep(ic,ae,pm).toString();ps(ad);ps(ac);ps(ab);oI(po.style,$,aa.toString());var pu=po;break;case 2:var pv=on(_),pw=pv.style;pw.backgroundImage=ep(ic,Z,pm).toString();var pu=pv;break;default:var px=i_(i7,bt);px.src=pm.toString();px.name=pm.toString();px.style.display=Y.toString();var pu=px;}else{var pA=pn[2],pz=pn[1],py=on(ak);py.style.overflow=aj.toString();var pB=on(ai),pO=function(pI,pN){var pE=pC/pD,pH=pF/pG,pJ=pB.style;pJ.width=ep(ic,ao,m4(pI.width*pE)).toString();var pK=pB.style;pK.width=ep(ic,an,m4(pI.height*pH)).toString();var pL=pB.style;pL.marginLeft=ep(ic,am,m4((-pz|0)*pE)).toString();var pM=pB.style;pM.marginTop=ep(ic,al,m4((-pA|0)*pH)).toString();return is;};pB.onload=iz(caml_js_wrap_meth_callback(function(pQ,pP){if(pP){var pR=pO(pQ,pP);if(!(pR|0))pP.preventDefault();return pR;}var pS=iG(0),pT=pO(pQ,pS);if(!(pT|0))pS.returnValue=pT;return pT;}));pB.src=pm.toString();pB.name=pm.toString();i1(py,pB);var pu=py;}return pu;case 1:return ep(o6,pk[1],pk[2]);case 2:return ep(o5,pk[1],pk[2]);case 3:var pW=pk[2],pV=pk[1],pU=on(ar),pX=pU.style;pU.innerHTML=pV.toString();je(pW,function(pY){switch(pY){case 1:var pZ=a3;break;case 2:var pZ=a2;break;case 3:var pZ=a1;break;case 4:var pZ=a0;break;case 5:var pZ=aZ;break;default:var pZ=a4;}return pX.textAlign=pZ.toString();});pX.visibility=aq.toString();pX.pointerEvents=ap.toString();return pU;default:throw [0,c,aP];}});caml_update_dummy(o8,function(p0){var p1=cY(o7,p0),p2=p0[1],p3=p2[7],p4=p2[6],p5=p2[4],p_=p2[9],p9=p2[8],p8=p2[5],p7=p2[3],p6=p1.style;p6.width=ep(ic,X,p2[2]).toString();var p$=p1.style;p$.height=ep(ic,W,p7).toString();if(p5!=1){var qa=p1.style;qa.opacity=io(p5.toString());}je(p8,function(qb){var qc=p1.style;return qc.backgroundColor=l3(qb).toString();});if(caml_string_notequal(p3,V))p1.id=p3.toString();if(caml_string_notequal(p4,U))o1(p1,p4);je(p9,cY(oZ,p1));je(p_,cY(o0,p1));return p1;});var qd=[];function qJ(qf,qe){var qg=cY(o8,qe);qf.parentNode.replaceChild(qg,qf);return 0;}caml_update_dummy(qd,function(qh,qk,qj){var qi=caml_equal(qh.tagName,aS.toString())?qh.firstChild:qh;if(qk[1][1]===qj[1][1]){if(qj[1][2]!==qk[1][2]){var ql=qi.style;ql.width=ep(ic,T,qj[1][2]).toString();}if(qj[1][3]!==qk[1][3]){var qm=qi.style;qm.height=ep(ic,S,qj[1][3]).toString();}if(qj[1][4]!=qk[1][4]){var qn=qi.style;qn.opacity=io(qj[1][4].toString());}var qo=qj[1][5],qp=qo?l3(qo[1]):R,qq=qp.toString();if(caml_notequal(qi.style.backgroundColor,qq)){var qs=qi.style,qr=caml_equal(qq,Q.toString())?P.toString():qq;qs.backgroundColor=qr;}if(caml_string_notequal(qj[1][7],qk[1][7]))qi.id=qj[1][7].toString();if(caml_string_notequal(qj[1][6],qk[1][6]))if(caml_string_notequal(qk[1][6],O))qi.lastChild.href=qj[1][6].toString();else o1(qi,qj[1][6]);var qt=qj[1][8];if(qt){var qu=qt[1];if(iq(qi.oakHoverHandler))qi.oakHoverHandler=io(qu);else oZ(qi,qu);}else{ir(qi.hoverOverId,i6);ir(qi.hoverOutId,i6);}var qv=qj[1][9];if(qv){var qw=qv[1],qx=iq(qi.oakClickHandler)?qi.oakClickHandler=io(qw):o0(qi,qw);}else var qx=ir(qi.clickId,i6);return qx;}var qy=qk[2],qz=qj[2];if(typeof qy==="number")var qD=typeof qz==="number"?1:0;else switch(qy[0]){case 1:if(typeof qz==="number"||!(1===qz[0]))var qD=0;else{var qA=qz[2],qB=qi.firstChild,qC=qz[1];dW(qd,qB,qy[2],qA);o2(qC,qA,qB);var qD=1;}break;case 2:if(typeof qz==="number"||!(2===qz[0]))var qD=0;else{var qE=qz[2],qF=qz[1],qH=qy[2];switch(qy[1]){case 1:var qG=1===qF?1:0;break;case 2:var qG=2===qF?1:0;break;case 3:var qG=3===qF?1:0;break;case 4:var qG=4===qF?1:0;break;case 5:var qG=5<=qF?1:0;break;default:var qG=0===qF?1:0;}if(qG){var qI=qi.childNodes;if(qE.length-1!==qI.length)qJ(qi,qj);else{var qK=o3(qF),qL=qE.length-1,qM=qL-1|0,qN=o4(qF);for(;;){if(0<=qM){var qQ=caml_array_get(qE,qM),qP=caml_array_get(qH,qM),qO=qN?(qL-qM|0)-1|0:qM;dW(qd,qI.item(qO),qP,qQ);cY(qK,qI.item(qM));var qR=qM-1|0,qM=qR;continue;}break;}}}else qJ(qi,qj);var qD=1;}break;case 3:if(typeof qz==="number"||!(3===qz[0]))var qD=0;else{var qS=qz[1];if(caml_string_notequal(qy[1],qS))qi.innerHTML=qS.toString();var qD=1;}break;case 4:var qD=0;break;default:if(typeof qz==="number"||!(0===qz[0]))var qD=0;else{var qT=qz[4],qU=qz[1],qV=qy[4];if(typeof qU==="number"&&0===qU){if(caml_string_notequal(qV,qT))oI(qi,aR,qT.toString());var qW=1;}else var qW=0;if(!qW){var qX=caml_notequal(qj[2],qk[2])?0:qj[1][2]!==qk[1][2]?0:qj[1][3]!==qk[1][3]?0:1;if(!qX)qJ(qi,qj);}var qD=1;}}if(qD)return 0;throw [0,c,aQ];});function rc(q1,qY){var qZ=qY[2],q0=qY[1];if(0<qZ[2]&&q1[2]==0){var q2=[0,q1[1],q1[2],q1[3],9,q1[5]],q3=1;}else var q3=0;if(!q3)var q2=q1;var q4=0<q2[2]?[0,q2[1],q2[2],q2[3],q2[4]-q0/3,q2[5]]:q2,q5=qZ[1],q6=0<=q5?0<q5?k:q4[5]:l,q7=q4[4],q8=0<q4[2]?q4[3]:4*q5,q9=q4[2]+q0*q7,q_=0,ra=q4[1],q$=caml_greaterequal(q_,q9)?q_:q9;return [0,ra+q0*q8,q$,q8,q7,q6];}var rb=d[2],rd=d[1];function rB(rf){var re=20,rg=cY(m6,[0,rf[1],rf[2]-25-re]),rh=2*re,ri=50,rj=2*mV/ri,ro=cY(m5,l2(0,255,0,0)),rm=rh/2,rl=rh/2;function rn(rk){return [0,rm*Math.cos(rj*rk),rl*Math.sin(rj*rk)];}if(0===ri)var rp=[0];else{var rq=caml_make_vect(ri,rn(0)),rr=1,rs=ri-1|0;if(!(rs<rr)){var rt=rr;for(;;){rq[rt+1]=rn(rt);var ru=rt+1|0;if(rs!==rt){var rt=ru;continue;}break;}}var rp=rq;}var rv=j3(j3(rp,ro),rg),rw=cY(m6,[0,rd/2,rb-12.5]),rx=cY(m5,l2(0,0,255,0)),ry=j3(j3(mQ(rd,25),rx),rw),rz=cY(m6,[0,rd/2,rb/2]),rA=cY(m5,l2(0,174,238,238));return m1([3,aY,[0,j3(j3(mQ(rd,rb),rA),rz),ry,rv]]);}var rD=j3(j3(i7.getElementById(j.toString()),ip),jv);function rH(rC){return rC/20;}var rG=1e3/30;function rJ(rF,rE){return rE-rF;}var rI=jS(0),rK=jS(0);function rP(rO){var rM=setInterval(caml_js_wrap_callback(function(rL){return kk([0,rI,rK],new iv().valueOf());}),rG);return function(rN){return clearInterval(rM);};}var rS=0,rR=jS(0),rT=lC(lR([0,[0,rP,function(rQ){return 0;},rI,rR,rK,rS]],rJ),rH),rU=jS(0),rV=jS(0);function rX(rW){return 0;}var rZ=lf(mF[1],rX),r0=[0,lf(rT,function(rY){return kk([0,rU,rV],[0,rY,mF[2][1]]);}),[0,rT]],r1=[0,[0,rZ,[0,mF[1]]],r0],r5=[1,[0,0,rU,jS(0),rV,r1]];function r3(r2){return [0,r2[1],rb-r2[2],r2[3],r2[4],r2[5]];}function r6(r4){return lS(r4,r3);}var r7=j3(lU(r5,i,rc),r6);j2(h,lT(r7));function r_(r8){return 0;}var r9=lS(r7,rB),r$=i_(i7,bu);if(1-(r$.getContext==ie?1:0)){var sa=r$.style;sa.width=ep(ic,x,rd).toString();var sb=r$.style;sb.height=ep(ic,w,rb).toString();r$.style.display=v.toString();r$.style.position=u.toString();r$.width=rd;r$.height=rb;var sc=r$.getContext(i$);i1(rD,r$);n0(sc,r9[2][1]);var sf=function(sd){return n0(sc,sd);},se=lT(r9),sg=lf(se,sf);kG(sg,se);j3(function(sh){return kY(sg,se);},r_);cg(0);return;}throw [0,ja];}());
