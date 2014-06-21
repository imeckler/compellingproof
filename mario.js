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
function caml_js_eval_string () {return eval(arguments[0].toString());}
function caml_js_to_array(a) { return [0].concat(a); }
function caml_js_wrap_callback(f) {
  var toArray = Array.prototype.slice;
  return function () {
    var args = (arguments.length > 0)?toArray.call (arguments):[undefined];
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
function caml_register_global (n, v) { caml_global_data[n + 1] = v; }
var caml_named_values = {};
function caml_register_named_value(nm,v) {
  caml_named_values[nm] = v; return 0;
}
function caml_sys_const_word_size () { return 32; }
(function(){function gJ(nf,ng,nh,ni,nj,nk,nl){return nf.length==6?nf(ng,nh,ni,nj,nk,nl):caml_call_gen(nf,[ng,nh,ni,nj,nk,nl]);}function jN(m$,na,nb,nc,nd,ne){return m$.length==5?m$(na,nb,nc,nd,ne):caml_call_gen(m$,[na,nb,nc,nd,ne]);}function cu(m7,m8,m9,m_){return m7.length==3?m7(m8,m9,m_):caml_call_gen(m7,[m8,m9,m_]);}function cZ(m4,m5,m6){return m4.length==2?m4(m5,m6):caml_call_gen(m4,[m5,m6]);}function bx(m2,m3){return m2.length==1?m2(m3):caml_call_gen(m2,[m3]);}var a=[0,new MlString("Failure")],b=[0,new MlString("Invalid_argument")],c=[0,600,400];caml_register_global(6,[0,new MlString("Not_found")]);caml_register_global(5,[0,new MlString("Division_by_zero")]);caml_register_global(3,b);caml_register_global(2,a);var aD=[0,new MlString("Assert_failure")],aC=new MlString("%d"),aB=new MlString("true"),aA=new MlString("false"),az=new MlString("Pervasives.do_at_exit"),ay=new MlString("\\b"),ax=new MlString("\\t"),aw=new MlString("\\n"),av=new MlString("\\r"),au=new MlString("\\\\"),at=new MlString("\\'"),as=new MlString("String.blit"),ar=new MlString("String.sub"),aq=new MlString("Buffer.add: cannot grow buffer"),ap=new MlString(""),ao=new MlString(""),an=new MlString("%.12g"),am=new MlString("\""),al=new MlString("\""),ak=new MlString("'"),aj=new MlString("'"),ai=new MlString("nan"),ah=new MlString("neg_infinity"),ag=new MlString("infinity"),af=new MlString("."),ae=new MlString("printf: bad positional specification (0)."),ad=new MlString("%_"),ac=[0,new MlString("printf.ml"),143,8],ab=new MlString("'"),aa=new MlString("Printf: premature end of format string '"),$=new MlString("'"),_=new MlString(" in format string '"),Z=new MlString(", at char number "),Y=new MlString("Printf: bad conversion %"),X=new MlString("Sformat.index_of_int: negative argument "),W=new MlString("canvas"),V=new MlString("2d"),U=new MlString("Dom_html.Canvas_not_available"),T=new MlString("Option.value_exn: None"),S=new MlString("(function(a,f){var len=a.length;for(var i = 0; i < len; ++i){f(a[i]);}})"),R=new MlString("iter"),Q=new MlString("iter"),P=new MlString("(function(t, x0, f){for(var k in t){if(t.hasOwnProperty(k)){x0=f(x0,parseInt(k),t[k]);}} return x0;})"),O=new MlString("(function(t, f){for(var k in t){if(t.hasOwnProperty(k)){f(parseInt(k),t[k]);}}})"),N=new MlString("(function(t, x0, f){for(var k in t){if(t.hasOwnProperty(k)){x0=f(k,t[k],x0);}} return x0;})"),M=new MlString("(function(t,f){for(var k in t){if(t.hasOwnProperty(k)){f(k,t[k]);}}})"),L=new MlString("(function(x,y){return x % y;})"),K=new MlString("Stream.turn_on_derived: Listener was not off or passive"),J=new MlString("Stream.turn_off_derived: Listener was not on"),I=new MlString("Stream.Prim.turn_off: Listener was not on or passive"),H=new MlString("Stream.Prim.turn_on: Listener was not off or passive"),G=new MlString("rgba(%d,%d,%d,%f)"),F=new MlString("pageY"),E=new MlString("pageX"),D=new MlString("mousemove"),C=new MlString("which"),B=new MlString("keyup"),A=new MlString("keydown"),z=new MlString("body"),y=new MlString("key_stream"),x=new MlString("%dpx"),w=new MlString("%dpx"),v=new MlString("block"),u=new MlString("absolute"),t=new MlString("repeat"),s=new MlString("butt"),r=new MlString("round"),q=new MlString("square"),p=new MlString("round"),o=new MlString("bevel"),n=new MlString("miter"),m=new MlString(""),l=[0,[0,0,0],[0,1,0,0,1]],k=new MlString("left"),j=new MlString("right"),i=new MlString("content"),h=[0,0,0,0,0,new MlString("right")],g=new MlString("mario_changes");function f(d){throw [0,a,d];}function aE(e){throw [0,b,e];}function aP(aF,aH){var aG=aF.getLen(),aI=aH.getLen(),aJ=caml_create_string(aG+aI|0);caml_blit_string(aF,0,aJ,0,aG);caml_blit_string(aH,0,aJ,aG,aI);return aJ;}function aQ(aK){return caml_format_int(aC,aK);}function aR(aO){var aL=caml_ml_out_channels_list(0);for(;;){if(aL){var aM=aL[2];try {}catch(aN){}var aL=aM;continue;}return 0;}}caml_register_named_value(az,aR);function a4(aS,aU){var aT=caml_create_string(aS);caml_fill_string(aT,0,aS,aU);return aT;}function a5(aX,aV,aW){if(0<=aV&&0<=aW&&!((aX.getLen()-aW|0)<aV)){var aY=caml_create_string(aW);caml_blit_string(aX,aV,aY,0,aW);return aY;}return aE(ar);}function a6(a1,a0,a3,a2,aZ){if(0<=aZ&&0<=a0&&!((a1.getLen()-aZ|0)<a0)&&0<=a2&&!((a3.getLen()-aZ|0)<a2))return caml_blit_string(a1,a0,a3,a2,aZ);return aE(as);}var a7=caml_sys_const_word_size(0),a8=caml_mul(a7/8|0,(1<<(a7-10|0))-1|0)-1|0;function bo(a9){var a_=1<=a9?a9:1,a$=a8<a_?a8:a_,ba=caml_create_string(a$);return [0,ba,0,a$,ba];}function bp(bb){return a5(bb[1],0,bb[2]);}function bi(bc,be){var bd=[0,bc[3]];for(;;){if(bd[1]<(bc[2]+be|0)){bd[1]=2*bd[1]|0;continue;}if(a8<bd[1])if((bc[2]+be|0)<=a8)bd[1]=a8;else f(aq);var bf=caml_create_string(bd[1]);a6(bc[1],0,bf,0,bc[2]);bc[1]=bf;bc[3]=bd[1];return 0;}}function bq(bg,bj){var bh=bg[2];if(bg[3]<=bh)bi(bg,1);bg[1].safeSet(bh,bj);bg[2]=bh+1|0;return 0;}function br(bm,bk){var bl=bk.getLen(),bn=bm[2]+bl|0;if(bm[3]<bn)bi(bm,bl);a6(bk,0,bm[1],bm[2],bl);bm[2]=bn;return 0;}function bv(bs){return 0<=bs?bs:f(aP(X,aQ(bs)));}function bw(bt,bu){return bv(bt+bu|0);}var by=bx(bw,1);function bF(bz){return a5(bz,0,bz.getLen());}function bH(bA,bB,bD){var bC=aP(_,aP(bA,$)),bE=aP(Z,aP(aQ(bB),bC));return aE(aP(Y,aP(a4(1,bD),bE)));}function cA(bG,bJ,bI){return bH(bF(bG),bJ,bI);}function cB(bK){return aE(aP(aa,aP(bF(bK),ab)));}function b8(bL,bT,bV,bX){function bS(bM){if((bL.safeGet(bM)-48|0)<0||9<(bL.safeGet(bM)-48|0))return bM;var bN=bM+1|0;for(;;){var bO=bL.safeGet(bN);if(48<=bO){if(!(58<=bO)){var bQ=bN+1|0,bN=bQ;continue;}var bP=0;}else if(36===bO){var bR=bN+1|0,bP=1;}else var bP=0;if(!bP)var bR=bM;return bR;}}var bU=bS(bT+1|0),bW=bo((bV-bU|0)+10|0);bq(bW,37);var bY=bX,bZ=0;for(;;){if(bY){var b0=bY[2],b1=[0,bY[1],bZ],bY=b0,bZ=b1;continue;}var b2=bU,b3=bZ;for(;;){if(b2<=bV){var b4=bL.safeGet(b2);if(42===b4){if(b3){var b5=b3[2];br(bW,aQ(b3[1]));var b6=bS(b2+1|0),b2=b6,b3=b5;continue;}throw [0,aD,ac];}bq(bW,b4);var b7=b2+1|0,b2=b7;continue;}return bp(bW);}}}function d3(cc,ca,b$,b_,b9){var cb=b8(ca,b$,b_,b9);if(78!==cc&&110!==cc)return cb;cb.safeSet(cb.getLen()-1|0,117);return cb;}function cC(cj,ct,cy,cd,cx){var ce=cd.getLen();function cv(cf,cs){var cg=40===cf?41:125;function cr(ch){var ci=ch;for(;;){if(ce<=ci)return bx(cj,cd);if(37===cd.safeGet(ci)){var ck=ci+1|0;if(ce<=ck)var cl=bx(cj,cd);else{var cm=cd.safeGet(ck),cn=cm-40|0;if(cn<0||1<cn){var co=cn-83|0;if(co<0||2<co)var cp=1;else switch(co){case 1:var cp=1;break;case 2:var cq=1,cp=0;break;default:var cq=0,cp=0;}if(cp){var cl=cr(ck+1|0),cq=2;}}else var cq=0===cn?0:1;switch(cq){case 1:var cl=cm===cg?ck+1|0:cu(ct,cd,cs,cm);break;case 2:break;default:var cl=cr(cv(cm,ck+1|0)+1|0);}}return cl;}var cw=ci+1|0,ci=cw;continue;}}return cr(cs);}return cv(cy,cx);}function c2(cz){return cu(cC,cB,cA,cz);}function dg(cD,cO,cY){var cE=cD.getLen()-1|0;function c0(cF){var cG=cF;a:for(;;){if(cG<cE){if(37===cD.safeGet(cG)){var cH=0,cI=cG+1|0;for(;;){if(cE<cI)var cJ=cB(cD);else{var cK=cD.safeGet(cI);if(58<=cK){if(95===cK){var cM=cI+1|0,cL=1,cH=cL,cI=cM;continue;}}else if(32<=cK)switch(cK-32|0){case 1:case 2:case 4:case 5:case 6:case 7:case 8:case 9:case 12:case 15:break;case 0:case 3:case 11:case 13:var cN=cI+1|0,cI=cN;continue;case 10:var cP=cu(cO,cH,cI,105),cI=cP;continue;default:var cQ=cI+1|0,cI=cQ;continue;}var cR=cI;c:for(;;){if(cE<cR)var cS=cB(cD);else{var cT=cD.safeGet(cR);if(126<=cT)var cU=0;else switch(cT){case 78:case 88:case 100:case 105:case 111:case 117:case 120:var cS=cu(cO,cH,cR,105),cU=1;break;case 69:case 70:case 71:case 101:case 102:case 103:var cS=cu(cO,cH,cR,102),cU=1;break;case 33:case 37:case 44:case 64:var cS=cR+1|0,cU=1;break;case 83:case 91:case 115:var cS=cu(cO,cH,cR,115),cU=1;break;case 97:case 114:case 116:var cS=cu(cO,cH,cR,cT),cU=1;break;case 76:case 108:case 110:var cV=cR+1|0;if(cE<cV){var cS=cu(cO,cH,cR,105),cU=1;}else{var cW=cD.safeGet(cV)-88|0;if(cW<0||32<cW)var cX=1;else switch(cW){case 0:case 12:case 17:case 23:case 29:case 32:var cS=cZ(cY,cu(cO,cH,cR,cT),105),cU=1,cX=0;break;default:var cX=1;}if(cX){var cS=cu(cO,cH,cR,105),cU=1;}}break;case 67:case 99:var cS=cu(cO,cH,cR,99),cU=1;break;case 66:case 98:var cS=cu(cO,cH,cR,66),cU=1;break;case 41:case 125:var cS=cu(cO,cH,cR,cT),cU=1;break;case 40:var cS=c0(cu(cO,cH,cR,cT)),cU=1;break;case 123:var c1=cu(cO,cH,cR,cT),c3=cu(c2,cT,cD,c1),c4=c1;for(;;){if(c4<(c3-2|0)){var c5=cZ(cY,c4,cD.safeGet(c4)),c4=c5;continue;}var c6=c3-1|0,cR=c6;continue c;}default:var cU=0;}if(!cU)var cS=cA(cD,cR,cT);}var cJ=cS;break;}}var cG=cJ;continue a;}}var c7=cG+1|0,cG=c7;continue;}return cG;}}c0(0);return 0;}function fg(dh){var c8=[0,0,0,0];function df(db,dc,c9){var c_=41!==c9?1:0,c$=c_?125!==c9?1:0:c_;if(c$){var da=97===c9?2:1;if(114===c9)c8[3]=c8[3]+1|0;if(db)c8[2]=c8[2]+da|0;else c8[1]=c8[1]+da|0;}return dc+1|0;}dg(dh,df,function(dd,de){return dd+1|0;});return c8[1];}function dZ(di,dl,dj){var dk=di.safeGet(dj);if((dk-48|0)<0||9<(dk-48|0))return cZ(dl,0,dj);var dm=dk-48|0,dn=dj+1|0;for(;;){var dp=di.safeGet(dn);if(48<=dp){if(!(58<=dp)){var ds=dn+1|0,dr=(10*dm|0)+(dp-48|0)|0,dm=dr,dn=ds;continue;}var dq=0;}else if(36===dp)if(0===dm){var dt=f(ae),dq=1;}else{var dt=cZ(dl,[0,bv(dm-1|0)],dn+1|0),dq=1;}else var dq=0;if(!dq)var dt=cZ(dl,0,dj);return dt;}}function dU(du,dv){return du?dv:bx(by,dv);}function dJ(dw,dx){return dw?dw[1]:dx;}function gI(fB,dz,fN,dC,fl,fT,dy){var dA=bx(dz,dy);function fC(dB){return cZ(dC,dA,dB);}function fk(dH,fS,dD,dM){var dG=dD.getLen();function fh(fK,dE){var dF=dE;for(;;){if(dG<=dF)return bx(dH,dA);var dI=dD.safeGet(dF);if(37===dI){var dQ=function(dL,dK){return caml_array_get(dM,dJ(dL,dK));},dW=function(dY,dR,dT,dN){var dO=dN;for(;;){var dP=dD.safeGet(dO)-32|0;if(!(dP<0||25<dP))switch(dP){case 1:case 2:case 4:case 5:case 6:case 7:case 8:case 9:case 12:case 15:break;case 10:return dZ(dD,function(dS,dX){var dV=[0,dQ(dS,dR),dT];return dW(dY,dU(dS,dR),dV,dX);},dO+1|0);default:var d0=dO+1|0,dO=d0;continue;}var d1=dD.safeGet(dO);if(124<=d1)var d2=0;else switch(d1){case 78:case 88:case 100:case 105:case 111:case 117:case 120:var d4=dQ(dY,dR),d5=caml_format_int(d3(d1,dD,dF,dO,dT),d4),d7=d6(dU(dY,dR),d5,dO+1|0),d2=1;break;case 69:case 71:case 101:case 102:case 103:var d8=dQ(dY,dR),d9=caml_format_float(b8(dD,dF,dO,dT),d8),d7=d6(dU(dY,dR),d9,dO+1|0),d2=1;break;case 76:case 108:case 110:var d_=dD.safeGet(dO+1|0)-88|0;if(d_<0||32<d_)var d$=1;else switch(d_){case 0:case 12:case 17:case 23:case 29:case 32:var ea=dO+1|0,eb=d1-108|0;if(eb<0||2<eb)var ec=0;else{switch(eb){case 1:var ec=0,ed=0;break;case 2:var ee=dQ(dY,dR),ef=caml_format_int(b8(dD,dF,ea,dT),ee),ed=1;break;default:var eg=dQ(dY,dR),ef=caml_format_int(b8(dD,dF,ea,dT),eg),ed=1;}if(ed){var eh=ef,ec=1;}}if(!ec){var ei=dQ(dY,dR),eh=caml_int64_format(b8(dD,dF,ea,dT),ei);}var d7=d6(dU(dY,dR),eh,ea+1|0),d2=1,d$=0;break;default:var d$=1;}if(d$){var ej=dQ(dY,dR),ek=caml_format_int(d3(110,dD,dF,dO,dT),ej),d7=d6(dU(dY,dR),ek,dO+1|0),d2=1;}break;case 37:case 64:var d7=d6(dR,a4(1,d1),dO+1|0),d2=1;break;case 83:case 115:var el=dQ(dY,dR);if(115===d1)var em=el;else{var en=[0,0],eo=0,ep=el.getLen()-1|0;if(!(ep<eo)){var eq=eo;for(;;){var er=el.safeGet(eq),es=14<=er?34===er?1:92===er?1:0:11<=er?13<=er?1:0:8<=er?1:0,et=es?2:caml_is_printable(er)?1:4;en[1]=en[1]+et|0;var eu=eq+1|0;if(ep!==eq){var eq=eu;continue;}break;}}if(en[1]===el.getLen())var ev=el;else{var ew=caml_create_string(en[1]);en[1]=0;var ex=0,ey=el.getLen()-1|0;if(!(ey<ex)){var ez=ex;for(;;){var eA=el.safeGet(ez),eB=eA-34|0;if(eB<0||58<eB)if(-20<=eB)var eC=1;else{switch(eB+34|0){case 8:ew.safeSet(en[1],92);en[1]+=1;ew.safeSet(en[1],98);var eD=1;break;case 9:ew.safeSet(en[1],92);en[1]+=1;ew.safeSet(en[1],116);var eD=1;break;case 10:ew.safeSet(en[1],92);en[1]+=1;ew.safeSet(en[1],110);var eD=1;break;case 13:ew.safeSet(en[1],92);en[1]+=1;ew.safeSet(en[1],114);var eD=1;break;default:var eC=1,eD=0;}if(eD)var eC=0;}else var eC=(eB-1|0)<0||56<(eB-1|0)?(ew.safeSet(en[1],92),en[1]+=1,ew.safeSet(en[1],eA),0):1;if(eC)if(caml_is_printable(eA))ew.safeSet(en[1],eA);else{ew.safeSet(en[1],92);en[1]+=1;ew.safeSet(en[1],48+(eA/100|0)|0);en[1]+=1;ew.safeSet(en[1],48+((eA/10|0)%10|0)|0);en[1]+=1;ew.safeSet(en[1],48+(eA%10|0)|0);}en[1]+=1;var eE=ez+1|0;if(ey!==ez){var ez=eE;continue;}break;}}var ev=ew;}var em=aP(al,aP(ev,am));}if(dO===(dF+1|0))var eF=em;else{var eG=b8(dD,dF,dO,dT);try {var eH=0,eI=1;for(;;){if(eG.getLen()<=eI)var eJ=[0,0,eH];else{var eK=eG.safeGet(eI);if(49<=eK)if(58<=eK)var eL=0;else{var eJ=[0,caml_int_of_string(a5(eG,eI,(eG.getLen()-eI|0)-1|0)),eH],eL=1;}else{if(45===eK){var eN=eI+1|0,eM=1,eH=eM,eI=eN;continue;}var eL=0;}if(!eL){var eO=eI+1|0,eI=eO;continue;}}var eP=eJ;break;}}catch(eQ){if(eQ[1]!==a)throw eQ;var eP=bH(eG,0,115);}var eR=eP[1],eS=em.getLen(),eT=0,eX=eP[2],eW=32;if(eR===eS&&0===eT){var eU=em,eV=1;}else var eV=0;if(!eV)if(eR<=eS)var eU=a5(em,eT,eS);else{var eY=a4(eR,eW);if(eX)a6(em,eT,eY,0,eS);else a6(em,eT,eY,eR-eS|0,eS);var eU=eY;}var eF=eU;}var d7=d6(dU(dY,dR),eF,dO+1|0),d2=1;break;case 67:case 99:var eZ=dQ(dY,dR);if(99===d1)var e0=a4(1,eZ);else{if(39===eZ)var e1=at;else if(92===eZ)var e1=au;else{if(14<=eZ)var e2=0;else switch(eZ){case 8:var e1=ay,e2=1;break;case 9:var e1=ax,e2=1;break;case 10:var e1=aw,e2=1;break;case 13:var e1=av,e2=1;break;default:var e2=0;}if(!e2)if(caml_is_printable(eZ)){var e3=caml_create_string(1);e3.safeSet(0,eZ);var e1=e3;}else{var e4=caml_create_string(4);e4.safeSet(0,92);e4.safeSet(1,48+(eZ/100|0)|0);e4.safeSet(2,48+((eZ/10|0)%10|0)|0);e4.safeSet(3,48+(eZ%10|0)|0);var e1=e4;}}var e0=aP(aj,aP(e1,ak));}var d7=d6(dU(dY,dR),e0,dO+1|0),d2=1;break;case 66:case 98:var e6=dO+1|0,e5=dQ(dY,dR)?aB:aA,d7=d6(dU(dY,dR),e5,e6),d2=1;break;case 40:case 123:var e7=dQ(dY,dR),e8=cu(c2,d1,dD,dO+1|0);if(123===d1){var e9=bo(e7.getLen()),fb=function(e$,e_){bq(e9,e_);return e$+1|0;};dg(e7,function(fa,fd,fc){if(fa)br(e9,ad);else bq(e9,37);return fb(fd,fc);},fb);var fe=bp(e9),d7=d6(dU(dY,dR),fe,e8),d2=1;}else{var ff=dU(dY,dR),fi=bw(fg(e7),ff),d7=fk(function(fj){return fh(fi,e8);},ff,e7,dM),d2=1;}break;case 33:bx(fl,dA);var d7=fh(dR,dO+1|0),d2=1;break;case 41:var d7=d6(dR,ap,dO+1|0),d2=1;break;case 44:var d7=d6(dR,ao,dO+1|0),d2=1;break;case 70:var fm=dQ(dY,dR);if(0===dT)var fn=an;else{var fo=b8(dD,dF,dO,dT);if(70===d1)fo.safeSet(fo.getLen()-1|0,103);var fn=fo;}var fp=caml_classify_float(fm);if(3===fp)var fq=fm<0?ah:ag;else if(4<=fp)var fq=ai;else{var fr=caml_format_float(fn,fm),fs=0,ft=fr.getLen();for(;;){if(ft<=fs)var fu=aP(fr,af);else{var fv=fr.safeGet(fs)-46|0,fw=fv<0||23<fv?55===fv?1:0:(fv-1|0)<0||21<(fv-1|0)?1:0;if(!fw){var fx=fs+1|0,fs=fx;continue;}var fu=fr;}var fq=fu;break;}}var d7=d6(dU(dY,dR),fq,dO+1|0),d2=1;break;case 91:var d7=cA(dD,dO,d1),d2=1;break;case 97:var fy=dQ(dY,dR),fz=bx(by,dJ(dY,dR)),fA=dQ(0,fz),fE=dO+1|0,fD=dU(dY,fz);if(fB)fC(cZ(fy,0,fA));else cZ(fy,dA,fA);var d7=fh(fD,fE),d2=1;break;case 114:var d7=cA(dD,dO,d1),d2=1;break;case 116:var fF=dQ(dY,dR),fH=dO+1|0,fG=dU(dY,dR);if(fB)fC(bx(fF,0));else bx(fF,dA);var d7=fh(fG,fH),d2=1;break;default:var d2=0;}if(!d2)var d7=cA(dD,dO,d1);return d7;}},fM=dF+1|0,fJ=0;return dZ(dD,function(fL,fI){return dW(fL,fK,fJ,fI);},fM);}cZ(fN,dA,dI);var fO=dF+1|0,dF=fO;continue;}}function d6(fR,fP,fQ){fC(fP);return fh(fR,fQ);}return fh(fS,0);}var fU=cZ(fk,fT,bv(0)),fV=fg(dy);if(fV<0||6<fV){var f8=function(fW,f2){if(fV<=fW){var fX=caml_make_vect(fV,0),f0=function(fY,fZ){return caml_array_set(fX,(fV-fY|0)-1|0,fZ);},f1=0,f3=f2;for(;;){if(f3){var f4=f3[2],f5=f3[1];if(f4){f0(f1,f5);var f6=f1+1|0,f1=f6,f3=f4;continue;}f0(f1,f5);}return cZ(fU,dy,fX);}}return function(f7){return f8(fW+1|0,[0,f7,f2]);};},f9=f8(0,0);}else switch(fV){case 1:var f9=function(f$){var f_=caml_make_vect(1,0);caml_array_set(f_,0,f$);return cZ(fU,dy,f_);};break;case 2:var f9=function(gb,gc){var ga=caml_make_vect(2,0);caml_array_set(ga,0,gb);caml_array_set(ga,1,gc);return cZ(fU,dy,ga);};break;case 3:var f9=function(ge,gf,gg){var gd=caml_make_vect(3,0);caml_array_set(gd,0,ge);caml_array_set(gd,1,gf);caml_array_set(gd,2,gg);return cZ(fU,dy,gd);};break;case 4:var f9=function(gi,gj,gk,gl){var gh=caml_make_vect(4,0);caml_array_set(gh,0,gi);caml_array_set(gh,1,gj);caml_array_set(gh,2,gk);caml_array_set(gh,3,gl);return cZ(fU,dy,gh);};break;case 5:var f9=function(gn,go,gp,gq,gr){var gm=caml_make_vect(5,0);caml_array_set(gm,0,gn);caml_array_set(gm,1,go);caml_array_set(gm,2,gp);caml_array_set(gm,3,gq);caml_array_set(gm,4,gr);return cZ(fU,dy,gm);};break;case 6:var f9=function(gt,gu,gv,gw,gx,gy){var gs=caml_make_vect(6,0);caml_array_set(gs,0,gt);caml_array_set(gs,1,gu);caml_array_set(gs,2,gv);caml_array_set(gs,3,gw);caml_array_set(gs,4,gx);caml_array_set(gs,5,gy);return cZ(fU,dy,gs);};break;default:var f9=cZ(fU,dy,[0]);}return f9;}function gH(gz){return bo(2*gz.getLen()|0);}function gE(gC,gA){var gB=bp(gA);gA[2]=0;return bx(gC,gB);}function gM(gD){var gG=bx(gE,gD);return gJ(gI,1,gH,bq,br,function(gF){return 0;},gG);}function gN(gL){return cZ(gM,function(gK){return gK;},gL);}var gO=[0,0];32===a7;var gP=null,gS=undefined,gT=Array;function gW(gQ){var gR=gQ==gP?0:[0,gQ];return gR;}var gV=Date;function gX(gU){return gU instanceof gT?0:[0,new MlWrappedString(gU.toString())];}gO[1]=[0,gX,gO[1]];var gY=this.document,g0=V.toString(),gZ=[0,U];this.HTMLElement===gS;function hg(g1){return g1?g1[1]:f(T);}function hf(g3,g6){var g2=0,g4=g3.length-1-1|0;if(!(g4<g2)){var g5=g2;for(;;){bx(g6,g3[g5+1]);var g7=g5+1|0;if(g4!==g5){var g5=g7;continue;}break;}}return 0;}function hh(g8,ha,hc){var g9=g8?g8[1]:function(g$,g_){return caml_equal(g$,g_);},hb=ha.length-1-1|0;for(;;){if(0<=hb){if(!cZ(g9,caml_array_get(ha,hb),hc)){var he=hb-1|0,hb=he;continue;}var hd=1;}else var hd=0;return hd;}}var hi=caml_js_eval_string(S);({}[R.toString()]=hi);var hj=caml_js_eval_string(P),hs={"iter":caml_js_eval_string(O),"fold":hj};function hD(hk){return {};}function hE(hl,hm,hn){return hl[hm]=hn;}function hF(ho,hp){return delete ho[hp];}function hG(hq,hr){return hq.hasOwnProperty(hr)|0?[0,hq[hr]]:0;}function hB(hx,hv){var hw=hs[Q.toString()];return hw(hx,caml_js_wrap_callback(function(hu,ht){return cZ(hv,hu,ht);}));}function hH(hC){var hy=[0,0];hB(hC,function(hz,hA){hy[1]+=1;return 0;});return hy[1];}var hI=caml_js_eval_string(N);({"iter":caml_js_eval_string(M),"fold":hI});caml_js_eval_string(L);function hN(hJ,hK){return window[hJ.toString()]=hK;}function hO(hL,hM){return bx(hM,hL);}function hT(hS,hP){return hB(hS,function(hR,hQ){return bx(hQ,hP);});}function h7(hW,hU){return hf(hW,function(hV){return hT(hV,hU);});}function iU(h0,hX,hZ){var hY=hX[6];hX[6]=hX[6]+1|0;hE(h0,hY,hZ);return hY;}function jC(h1,id){var h2=h1?h1[1]:function(h3,h4){return 0;},h5=hD(0),h6=hD(0);function h9(h8){return h7([0,h5,h6],h8);}var ib=0,ia=hD(0);function ic(h_){return 0;}return [0,[0,function(h$){return bx(h2,h9);},ic,h5,ia,h6,ib]];}function it(ii,ie){{if(0===ie[0]){var ig=ie[1],ik=function(ij,ih){hE(ig[3],ii,ih);hF(ij,ii);return 1===hH(ig[3])?(ig[2]=bx(ig[1],0),0):0;},il=hG(ig[4],ii);if(il)var im=ik(ig[4],il[1]);else{var io=hG(ig[5],ii),im=io?ik(ig[5],io[1]):f(H);}return im;}var ip=ie[1],iv=function(ir,iq){hE(ip[2],ii,iq);hF(ir,ii);if(1===hH(ip[2])){var iu=function(is){return it(is[1],is[2][1]);};return hf(ip[5],iu);}return 0;},iw=hG(ip[3],ii);if(iw)var ix=iv(ip[3],iw[1]);else{var iy=hG(ip[4],ii),ix=iy?iv(ip[4],iy[1]):f(K);}return ix;}}function iL(iC,iz){{if(0===iz[0]){var iA=iz[1],iE=function(iD,iB){hE(iA[4],iC,iB);return hF(iD,iC);},iF=hG(iA[3],iC);if(iF){iE(iA[3],iF[1]);var iG=0===hH(iA[3])?bx(iA[2],0):0;}else{var iH=hG(iA[5],iC),iG=iH?iE(iA[5],iH[1]):f(I);}return iG;}var iI=iz[1],iJ=hG(iI[2],iC);if(iJ){hE(iI[3],iC,iJ[1]);hF(iI[2],iC);if(0===hH(iI[2])){var iM=function(iK){return iL(iK[1],iK[2][1]);},iN=hf(iI[5],iM);}else var iN=0;}else var iN=f(J);return iN;}}function iX(iR,iO,iQ){var iP=iO[1];iO[1]=iO[1]+1|0;hE(iR,iP,iQ);return iP;}function i4(iS,iV){{if(0===iS[0]){var iT=iS[1];return iU(iT[4],iT,iV);}var iW=iS[1];return iX(iW[3],iW,iV);}}function i_(i5,i3){var iY=hD(0),iZ=hD(0);function i2(i0){hT(iY,i0);return hT(iZ,i0);}var i6=[0,[0,i4(i5,function(i1){return cZ(i3,i2,i1);}),[0,i5]]];return [1,[0,0,iY,hD(0),iZ,i6]];}function jo(i$,i8){return i_(i$,function(i9,i7){return bx(i9,bx(i8,i7));});}function jD(jf,jd){var ja=[0,0];return i_(jf,function(je,jc){var jb=ja[1];if(jb)bx(je,cZ(jd,jb[1],jc));ja[1]=[0,jc];return 0;});}function js(jg,jj){function ji(jh){jg[1]=jh;return 0;}if(0===jj[0]){var jk=jj[1],jl=iU(jk[5],jk,ji);}else{var jm=jj[1],jl=iX(jm[4],jm,ji);}return [0,jj,jg,[0,jl]];}function jE(jn,jp){var jq=jn[2],jr=jo(jn[1],jp);return js([0,bx(jp,jq[1])],jr);}function jF(jt){return jt[1];}function jG(jB,ju,jx){var jv=[0,ju],jA=[0,ju];return js(jA,i_(jB,function(jz,jw){var jy=cZ(jx,jv[1],jw);jv[1]=jy;return bx(jz,jy);}));}function jO(jH,jL,jK,jJ){var jI=jH?jH[1]:1;return [0,jL,jK,jJ,jI];}function jP(jM){return jN(gN,G,jM[1],jM[2],jM[3],jM[4]);}function jR(jQ){return jQ;}var jS=jQuery(z.toString());function j7(j4,jV){function jU(jT){return [0,jT[1],caml_js_wrap_callback(jT[2])];}var jW=jV.length-1;if(0===jW)var jX=[0];else{var jY=caml_make_vect(jW,jU(jV[0+1])),jZ=1,j0=jW-1|0;if(!(j0<jZ)){var j1=jZ;for(;;){jY[j1+1]=jU(jV[j1+1]);var j2=j1+1|0;if(j0!==j1){var j1=j2;continue;}break;}}var jX=jY;}hf(jX,function(j3){return j4.on(j3[1].toString(),j3[2]);});return function(j6){return hf(jX,function(j5){return j4.off(j5[1].toString(),j5[2]);});};}var kc=0,kd=jC([0,function(j$){function j_(j8){return j8[C.toString()];}var kb=[0,B,function(j9){j9.preventDefault();return bx(j$,[0,19067,j_(j9)]);}];return j7(jS,[0,[0,A,function(ka){ka.preventDefault();return bx(j$,[0,759637122,j_(ka)]);}],kb]);}],kc);hN(y,kd);var ke=hD(0),kq=jG(kd,[0],function(kj,kf){if(759637122<=kf[1])hE(ke,kf[2],0);else hF(ke,kf[2]);var kg=new gT();hB(ke,function(kh,ki){return kg.push(kh);});return caml_js_to_array(kg);});function km(kk){return 0===kk?0:1;}var kr=jE(kq,function(kl){var kn=km(hh(0,kl,jR(40))),ko=km(hh(0,kl,jR(38)))-kn|0,kp=km(hh(0,kl,jR(37)));return [0,km(hh(0,kl,jR(39)))-kp|0,ko];}),ku=0,kx=jC([0,function(kt){return j7(jS,[0,[0,D,function(ks){return bx(kt,[0,ks[E.toString()],ks[F.toString()]]);}]]);}],ku);jD(kx,function(kw,kv){return [0,kv[1]-kw[1]|0,kv[2]-kw[2]|0];});function kC(ky,kA){var kz=ky/2,kB=kA/2;return [0,[0,-kz,-kB],[0,-kz,kB],[0,kz,kB],[0,kz,-kB]];}var lD=4*Math.atan(1);function kG(kD){return [0,0,1,0,0,1,kD];}function lE(kE,kF){return kG([1,[1,[0,kE]],kF]);}function lF(kH,kI){return [0,kI[1],kI[2],kI[3]+kH[1],kI[4]+kH[2],kI[5],kI[6]];}function k2(kL,kJ,kR){if(1<kJ.length-1){var kK=caml_array_get(kJ,0);kL.moveTo(kK[1],kK[2]);var kM=1,kN=kJ.length-1-1|0;if(!(kN<kM)){var kO=kM;for(;;){var kP=caml_array_get(kJ,kO);kL.lineTo(kP[1],kP[2]);var kQ=kO+1|0;if(kN!==kO){var kO=kQ;continue;}break;}}if(kR){var kS=caml_array_get(kJ,0);return kL.lineTo(kS[1],kS[2]);}return 0;}return 0;}function le(kU,kT,k4,k3){kU.lineWidth=kT[2];var kV=kT[3],kW=416330352===kV?q:781662169<=kV?s:r;kU.lineCap=kW.toString();var kX=kT[4],kY=typeof kX==="number"?1006599246<=kX?p:o:n;kU.lineJoin=kY.toString();var kZ=kT[4];if(typeof kZ==="number"||!(256529610===kZ[1]))var k0=0;else{var k1=kZ[2],k0=1;}if(!k0)var k1=10;kU.miterLimit=k1;kU.strokeStyle=jP(kT[1]).toString();if(0===kT[5].length-1)k2(kU,k4,k3);else if(1<k4.length-1){var k5=caml_array_get(k4,k4.length-1-1|0);kU.moveTo(k5[1],k5[2]);f(m);}kU.scale(1,-1);return kU.stroke();}function lC(lb,k6){var k7=k6[6],k8=k6[5],k9=k6[4],k_=k6[3],k$=k6[2],la=k6[1];lb.save();var lc=k_!=0?0:k9!=0?0:1;if(!lc)lb.translate(k_,k9);if(la!=0)lb.rotate(la);if(k$!=1)lb.scale(k$,k$);if(k8!=1)lb.globalAlpha=lb.globalAlpha*k8;lb.beginPath();switch(k7[0]){case 1:var ld=k7[1];if(0===ld[0])le(lb,ld[1],k7[2],1);else{var lf=ld[1];k2(lb,k7[2],0);switch(lf[0]){case 1:lb.fillStyle=lb.createPattern(lf[1],t.toString());break;case 2:var lg=lf[1];if(0===lg[0]){var lh=lg[2],li=lg[1],lj=lg[3],lk=[0,lb.createLinearGradient(li[1],-li[2],lh[1],-lh[2]),lj];}else{var ll=lg[3],lm=lg[1],ln=lg[5],lk=[0,lb.createRadialGradient(lm[1],-lm[2],lg[2],ll[1],-ll[2],lg[4]),ln];}var lo=lk[1],lr=lk[2];hf(lr,function(lp){var lq=lp[1];return lo.addColorStop(lq,jP(lp[2]).toString());});lb.fillStyle=lo;break;default:lb.fillStyle=jP(lf[1]).toString();}lb.scale(1,-1);lb.fill();}break;case 2:var ls=k7[3],lt=k7[2],lu=k7[1],lx=k7[4],lw=ls[2],lv=ls[1];lb.scale(1,-1);lb.drawImage(lx,lv,lw,lu,lt,-lu/2,-lt/2,lu,lt);break;case 3:var ly=k7[1],lz=ly[2],lA=ly[1],lB=k7[2];lb.transform(lz[1],lz[2],lz[3],lz[4],lA[1],lA[2]);hf(lB,bx(lC,lb));break;default:le(lb,k7[1],k7[2],0);}return lb.restore();}function lW(lJ,lG){var lH=lG[2],lI=lG[1];if(0<lH[2]&&lJ[2]==0){var lK=[0,lJ[1],lJ[2],lJ[3],9,lJ[5]],lL=1;}else var lL=0;if(!lL)var lK=lJ;var lM=0<lK[2]?[0,lK[1],lK[2],lK[3],lK[4]-lI/3,lK[5]]:lK,lN=lH[1],lO=0<=lN?0<lN?j:lM[5]:k,lP=lM[4],lQ=0<lM[2]?lM[3]:4*lN,lR=lM[2]+lI*lP,lS=0,lU=lM[1],lT=caml_greaterequal(lS,lR)?lS:lR;return [0,lU+lI*lQ,lT,lQ,lP,lO];}var lV=c[2],lX=c[1];function mj(lZ){var lY=20,l0=bx(lF,[0,lZ[1],lZ[2]-25-lY]),l1=2*lY,l2=50,l3=2*lD/l2,l8=bx(lE,jO(0,255,0,0)),l6=l1/2,l5=l1/2;function l7(l4){return [0,l6*Math.cos(l3*l4),l5*Math.sin(l3*l4)];}if(0===l2)var l9=[0];else{var l_=caml_make_vect(l2,l7(0)),l$=1,ma=l2-1|0;if(!(ma<l$)){var mb=l$;for(;;){l_[mb+1]=l7(mb);var mc=mb+1|0;if(ma!==mb){var mb=mc;continue;}break;}}var l9=l_;}var md=hO(hO(l9,l8),l0),me=bx(lF,[0,lX/2,lV-12.5]),mf=bx(lE,jO(0,0,255,0)),mg=hO(hO(kC(lX,25),mf),me),mh=bx(lF,[0,lX/2,lV/2]),mi=bx(lE,jO(0,174,238,238));return kG([3,l,[0,hO(hO(kC(lX,lV),mi),mh),mg,md]]);}var ml=hO(hO(gY.getElementById(i.toString()),gW),hg);function mp(mk){return mk/20;}var mo=1e3/30;function mr(mn,mm){return mm-mn;}var mq=hD(0),ms=hD(0);function mx(mw){var mu=setInterval(caml_js_wrap_callback(function(mt){return h7([0,mq,ms],new gV().valueOf());}),mo);return function(mv){return clearInterval(mu);};}var mA=0,mz=hD(0),mB=jo(jD([0,[0,mx,function(my){return 0;},mq,mz,ms,mA]],mr),mp),mC=hD(0),mD=hD(0);function mF(mE){return 0;}var mH=i4(kr[1],mF),mI=[0,i4(mB,function(mG){return h7([0,mC,mD],[0,mG,kr[2][1]]);}),[0,mB]],mJ=[0,[0,mH,[0,kr[1]]],mI],mN=[1,[0,0,mC,hD(0),mD,mJ]];function mL(mK){return [0,mK[1],lV-mK[2],mK[3],mK[4],mK[5]];}function mO(mM){return jE(mM,mL);}var mP=hO(jG(mN,h,lW),mO);hN(g,jF(mP));function mS(mQ){return 0;}var mR=jE(mP,mj),mT=gY.createElement(W.toString());if(1-(mT.getContext==gP?1:0)){var mU=mT.style;mU.width=cZ(gN,x,lX).toString();var mV=mT.style;mV.height=cZ(gN,w,lV).toString();mT.style.display=v.toString();mT.style.position=u.toString();mT.width=lX;mT.height=lV;var mW=mT.getContext(g0);ml.appendChild(mT);lC(mW,mR[2][1]);var mZ=function(mX){return lC(mW,mX);},mY=jF(mR),m0=i4(mY,mZ);it(m0,mY);hO(function(m1){return iL(m0,mY);},mS);aR(0);return;}throw [0,gZ];}());
