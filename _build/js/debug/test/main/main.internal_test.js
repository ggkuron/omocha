function $bound_check(arr, index) {
  if (index < 0 || index >= arr.length) throw new Error("Index out of bounds");
}
class $PanicError extends Error {}
function $panic() {
  throw new $PanicError();
}
function $compare_int(a, b) {
  return (a >= b) - (a <= b);
}
function Result$Err$0$(param0) {
  this._0 = param0;
}
Result$Err$0$.prototype.$tag = 0;
Result$Err$0$.prototype.$name = "Err";
function Result$Ok$0$(param0) {
  this._0 = param0;
}
Result$Ok$0$.prototype.$tag = 1;
Result$Ok$0$.prototype.$name = "Ok";
function Error$astrata$47$astrata$47$main$46$MoonBitTestDriverInternalJsError$46$MoonBitTestDriverInternalJsError(param0) {
  this._0 = param0;
}
Error$astrata$47$astrata$47$main$46$MoonBitTestDriverInternalJsError$46$MoonBitTestDriverInternalJsError.prototype.$tag = 6;
Error$astrata$47$astrata$47$main$46$MoonBitTestDriverInternalJsError$46$MoonBitTestDriverInternalJsError.prototype.$name = "astrata/astrata/main.MoonBitTestDriverInternalJsError.MoonBitTestDriverInternalJsError";
function Error$moonbitlang$47$core$47$builtin$46$SnapshotError$46$SnapshotError(param0) {
  this._0 = param0;
}
Error$moonbitlang$47$core$47$builtin$46$SnapshotError$46$SnapshotError.prototype.$tag = 5;
Error$moonbitlang$47$core$47$builtin$46$SnapshotError$46$SnapshotError.prototype.$name = "moonbitlang/core/builtin.SnapshotError.SnapshotError";
function Error$moonbitlang$47$core$47$builtin$46$InspectError$46$InspectError(param0) {
  this._0 = param0;
}
Error$moonbitlang$47$core$47$builtin$46$InspectError$46$InspectError.prototype.$tag = 4;
Error$moonbitlang$47$core$47$builtin$46$InspectError$46$InspectError.prototype.$name = "moonbitlang/core/builtin.InspectError.InspectError";
function Error$astrata$47$astrata$47$main$46$MoonBitTestDriverInternalSkipTest$46$MoonBitTestDriverInternalSkipTest(param0) {
  this._0 = param0;
}
Error$astrata$47$astrata$47$main$46$MoonBitTestDriverInternalSkipTest$46$MoonBitTestDriverInternalSkipTest.prototype.$tag = 3;
Error$astrata$47$astrata$47$main$46$MoonBitTestDriverInternalSkipTest$46$MoonBitTestDriverInternalSkipTest.prototype.$name = "astrata/astrata/main.MoonBitTestDriverInternalSkipTest.MoonBitTestDriverInternalSkipTest";
function Error$moonbitlang$47$core$47$builtin$46$Failure$46$Failure(param0) {
  this._0 = param0;
}
Error$moonbitlang$47$core$47$builtin$46$Failure$46$Failure.prototype.$tag = 2;
Error$moonbitlang$47$core$47$builtin$46$Failure$46$Failure.prototype.$name = "moonbitlang/core/builtin.Failure.Failure";
const Error$moonbitlang$47$core$47$builtin$46$CreatingViewError$46$IndexOutOfBounds = { $tag: 1, $name: "moonbitlang/core/builtin.CreatingViewError.IndexOutOfBounds" };
const Error$moonbitlang$47$core$47$builtin$46$CreatingViewError$46$InvalidIndex = { $tag: 0, $name: "moonbitlang/core/builtin.CreatingViewError.InvalidIndex" };
const _M0FP311moonbitlang4core7builtin19int__to__string__js = (x, radix) => {
  return x.toString(radix);
};
function Result$Err$1$(param0) {
  this._0 = param0;
}
Result$Err$1$.prototype.$tag = 0;
Result$Err$1$.prototype.$name = "Err";
function Result$Ok$1$(param0) {
  this._0 = param0;
}
Result$Ok$1$.prototype.$tag = 1;
Result$Ok$1$.prototype.$name = "Ok";
const _M0MP37astrata7astrata4main34MoonBitTestDriverInternalTestEntry8filename = (entry) => entry[0];
const _M0MP37astrata7astrata4main34MoonBitTestDriverInternalTestEntry5index = (entry) => entry[1];
const _M0FP095_40moonbitlang_2fcore_2fbuiltin_2eStringBuilder_24as_24_40moonbitlang_2fcore_2fbuiltin_2eLogger = { method_0: _M0IP311moonbitlang4core7builtin13StringBuilderP311moonbitlang4core7builtin6Logger13write__string, method_1: _M0IP016_24default__implP311moonbitlang4core7builtin6Logger16write__substringGRP311moonbitlang4core7builtin13StringBuilderE, method_2: _M0IP311moonbitlang4core7builtin13StringBuilderP311moonbitlang4core7builtin6Logger11write__view, method_3: _M0IP311moonbitlang4core7builtin13StringBuilderP311moonbitlang4core7builtin6Logger11write__char };
function _M0FP15Error10to__string(_e) {
  switch (_e.$tag) {
    case 0: {
      return _M0IP016_24default__implP311moonbitlang4core7builtin4Show10to__stringGRP311moonbitlang4core7builtin17CreatingViewErrorE(_e);
    }
    case 1: {
      return _M0IP016_24default__implP311moonbitlang4core7builtin4Show10to__stringGRP311moonbitlang4core7builtin17CreatingViewErrorE(_e);
    }
    case 5: {
      return "moonbitlang/core/builtin.SnapshotError.SnapshotError";
    }
    case 6: {
      return "astrata/astrata/main.MoonBitTestDriverInternalJsError.MoonBitTestDriverInternalJsError";
    }
    case 2: {
      return _M0IP016_24default__implP311moonbitlang4core7builtin4Show10to__stringGRP311moonbitlang4core7builtin7FailureE(_e);
    }
    case 4: {
      return "moonbitlang/core/builtin.InspectError.InspectError";
    }
    default: {
      return "astrata/astrata/main.MoonBitTestDriverInternalSkipTest.MoonBitTestDriverInternalSkipTest";
    }
  }
}
function _M0MP311moonbitlang4core7builtin6Logger13write__objectGsE(self, obj) {
  _M0IP311moonbitlang4core6string6StringP311moonbitlang4core7builtin4Show6output(obj, self);
}
function _M0IP311moonbitlang4core7builtin17CreatingViewErrorP311moonbitlang4core7builtin4Show6output(_x_5142, _x_5143) {
  if (_x_5142.$tag === 1) {
    _x_5143.method_table.method_0(_x_5143.self, "IndexOutOfBounds");
    return;
  } else {
    _x_5143.method_table.method_0(_x_5143.self, "InvalidIndex");
    return;
  }
}
function _M0IP311moonbitlang4core7builtin7FailureP311moonbitlang4core7builtin4Show6output(_x_5126, _x_5127) {
  const _Failure = _x_5126;
  const _$42$arg_5128 = _Failure._0;
  _x_5127.method_table.method_0(_x_5127.self, "Failure(");
  _M0MP311moonbitlang4core7builtin6Logger13write__objectGsE(_x_5127, _$42$arg_5128);
  _x_5127.method_table.method_0(_x_5127.self, ")");
}
function _M0MP311moonbitlang4core4byte4Byte8to__char(self) {
  return self;
}
function _M0MP311moonbitlang4core7builtin13StringBuilder11new_2einner(size_hint) {
  return { val: "" };
}
function _M0MP311moonbitlang4core7builtin13StringBuilder10to__string(self) {
  return self.val;
}
function _M0IP311moonbitlang4core7builtin13StringBuilderP311moonbitlang4core7builtin6Logger11write__char(self, ch) {
  const _bind = self;
  _bind.val = `${_bind.val}${String.fromCodePoint(ch)}`;
}
function _M0MP311moonbitlang4core6uint166UInt1623is__trailing__surrogate(self) {
  return _M0IP016_24default__implP311moonbitlang4core7builtin7Compare6op__geGkE(self, 56320) && _M0IP016_24default__implP311moonbitlang4core7builtin7Compare6op__leGkE(self, 57343);
}
function _M0MP311moonbitlang4core6uint166UInt1616unsafe__to__char(self) {
  return self;
}
function _M0MP311moonbitlang4core6string6String6escape(self) {
  const buf = _M0MP311moonbitlang4core7builtin13StringBuilder11new_2einner(0);
  _M0IP311moonbitlang4core6string6StringP311moonbitlang4core7builtin4Show6output(self, { self: buf, method_table: _M0FP095_40moonbitlang_2fcore_2fbuiltin_2eStringBuilder_24as_24_40moonbitlang_2fcore_2fbuiltin_2eLogger });
  return _M0MP311moonbitlang4core7builtin13StringBuilder10to__string(buf);
}
function _M0MP311moonbitlang4core5array5Array2atGRP37astrata7astrata4main34MoonBitTestDriverInternalTestEntryE(self, index) {
  const len = self.length;
  if (index >= 0 && index < len) {
    $bound_check(self, index);
    return self[index];
  } else {
    return $panic();
  }
}
function _M0IP311moonbitlang4core7builtin13StringBuilderP311moonbitlang4core7builtin6Logger13write__string(self, str) {
  const _bind = self;
  _bind.val = `${_bind.val}${str}`;
}
function _M0IP311moonbitlang4core6uint166UInt16P311moonbitlang4core7builtin7Compare7compare(self, that) {
  return $compare_int(self, that);
}
function _M0IP016_24default__implP311moonbitlang4core7builtin7Compare6op__ltGkE(x, y) {
  return _M0IP311moonbitlang4core6uint166UInt16P311moonbitlang4core7builtin7Compare7compare(x, y) < 0;
}
function _M0IP016_24default__implP311moonbitlang4core7builtin7Compare6op__leGkE(x, y) {
  return _M0IP311moonbitlang4core6uint166UInt16P311moonbitlang4core7builtin7Compare7compare(x, y) <= 0;
}
function _M0IP016_24default__implP311moonbitlang4core7builtin7Compare6op__geGkE(x, y) {
  return _M0IP311moonbitlang4core6uint166UInt16P311moonbitlang4core7builtin7Compare7compare(x, y) >= 0;
}
function _M0MP311moonbitlang4core6string6String11sub_2einner(self, start, end) {
  const len = self.length;
  let end$2;
  if (end === undefined) {
    end$2 = len;
  } else {
    const _Some = end;
    const _end = _Some;
    end$2 = _end < 0 ? len + _end | 0 : _end;
  }
  const start$2 = start < 0 ? len + start | 0 : start;
  if (start$2 >= 0 && (start$2 <= end$2 && end$2 <= len)) {
    if (start$2 < len && _M0MP311moonbitlang4core6uint166UInt1623is__trailing__surrogate(self.charCodeAt(start$2))) {
      return new Result$Err$0$(Error$moonbitlang$47$core$47$builtin$46$CreatingViewError$46$InvalidIndex);
    }
    if (end$2 < len && _M0MP311moonbitlang4core6uint166UInt1623is__trailing__surrogate(self.charCodeAt(end$2))) {
      return new Result$Err$0$(Error$moonbitlang$47$core$47$builtin$46$CreatingViewError$46$InvalidIndex);
    }
    return new Result$Ok$0$({ str: self, start: start$2, end: end$2 });
  } else {
    return new Result$Err$0$(Error$moonbitlang$47$core$47$builtin$46$CreatingViewError$46$IndexOutOfBounds);
  }
}
function _M0IP016_24default__implP311moonbitlang4core7builtin6Logger16write__substringGRP311moonbitlang4core7builtin13StringBuilderE(self, value, start, len) {
  let _tmp;
  let _try_err;
  _L: {
    _L$2: {
      const _bind = _M0MP311moonbitlang4core6string6String11sub_2einner(value, start, start + len | 0);
      if (_bind.$tag === 1) {
        const _ok = _bind;
        _tmp = _ok._0;
      } else {
        const _err = _bind;
        _try_err = _err._0;
        break _L$2;
      }
      break _L;
    }
    _tmp = $panic();
  }
  _M0IP311moonbitlang4core7builtin13StringBuilderP311moonbitlang4core7builtin6Logger11write__view(self, _tmp);
}
function _M0IP016_24default__implP311moonbitlang4core7builtin4Show10to__stringGiE(self) {
  const logger = _M0MP311moonbitlang4core7builtin13StringBuilder11new_2einner(0);
  _M0IP311moonbitlang4core3int3IntP311moonbitlang4core7builtin4Show6output(self, { self: logger, method_table: _M0FP095_40moonbitlang_2fcore_2fbuiltin_2eStringBuilder_24as_24_40moonbitlang_2fcore_2fbuiltin_2eLogger });
  return _M0MP311moonbitlang4core7builtin13StringBuilder10to__string(logger);
}
function _M0IP016_24default__implP311moonbitlang4core7builtin4Show10to__stringGRP311moonbitlang4core7builtin7FailureE(self) {
  const logger = _M0MP311moonbitlang4core7builtin13StringBuilder11new_2einner(0);
  _M0IP311moonbitlang4core7builtin7FailureP311moonbitlang4core7builtin4Show6output(self, { self: logger, method_table: _M0FP095_40moonbitlang_2fcore_2fbuiltin_2eStringBuilder_24as_24_40moonbitlang_2fcore_2fbuiltin_2eLogger });
  return _M0MP311moonbitlang4core7builtin13StringBuilder10to__string(logger);
}
function _M0IP016_24default__implP311moonbitlang4core7builtin4Show10to__stringGRP311moonbitlang4core7builtin17CreatingViewErrorE(self) {
  const logger = _M0MP311moonbitlang4core7builtin13StringBuilder11new_2einner(0);
  _M0IP311moonbitlang4core7builtin17CreatingViewErrorP311moonbitlang4core7builtin4Show6output(self, { self: logger, method_table: _M0FP095_40moonbitlang_2fcore_2fbuiltin_2eStringBuilder_24as_24_40moonbitlang_2fcore_2fbuiltin_2eLogger });
  return _M0MP311moonbitlang4core7builtin13StringBuilder10to__string(logger);
}
function _M0MP311moonbitlang4core3int3Int18to__string_2einner(self, radix) {
  return _M0FP311moonbitlang4core7builtin19int__to__string__js(self, radix);
}
function _M0IP311moonbitlang4core4byte4ByteP311moonbitlang4core7builtin3Add3add(self, that) {
  return (self + that | 0) & 255;
}
function _M0IP311moonbitlang4core4byte4ByteP311moonbitlang4core7builtin3Div3div(self, that) {
  if (that === 0) {
    $panic();
  }
  return (self / that | 0) & 255;
}
function _M0IP311moonbitlang4core4byte4ByteP311moonbitlang4core7builtin3Mod3mod(self, that) {
  if (that === 0) {
    $panic();
  }
  return (self % that | 0) & 255;
}
function _M0IP311moonbitlang4core4byte4ByteP311moonbitlang4core7builtin3Sub3sub(self, that) {
  return (self - that | 0) & 255;
}
function _M0FP311moonbitlang4core7builtin31to__hex_2eto__hex__digit_7c3855(i) {
  return i < 10 ? _M0MP311moonbitlang4core4byte4Byte8to__char(_M0IP311moonbitlang4core4byte4ByteP311moonbitlang4core7builtin3Add3add(i, 48)) : _M0MP311moonbitlang4core4byte4Byte8to__char(_M0IP311moonbitlang4core4byte4ByteP311moonbitlang4core7builtin3Sub3sub(_M0IP311moonbitlang4core4byte4ByteP311moonbitlang4core7builtin3Add3add(i, 97), 10));
}
function _M0MP311moonbitlang4core4byte4Byte7to__hex(b) {
  const _self = _M0MP311moonbitlang4core7builtin13StringBuilder11new_2einner(0);
  _M0IP311moonbitlang4core7builtin13StringBuilderP311moonbitlang4core7builtin6Logger11write__char(_self, _M0FP311moonbitlang4core7builtin31to__hex_2eto__hex__digit_7c3855(_M0IP311moonbitlang4core4byte4ByteP311moonbitlang4core7builtin3Div3div(b, 16)));
  _M0IP311moonbitlang4core7builtin13StringBuilderP311moonbitlang4core7builtin6Logger11write__char(_self, _M0FP311moonbitlang4core7builtin31to__hex_2eto__hex__digit_7c3855(_M0IP311moonbitlang4core4byte4ByteP311moonbitlang4core7builtin3Mod3mod(b, 16)));
  return _M0MP311moonbitlang4core7builtin13StringBuilder10to__string(_self);
}
function _M0FP311moonbitlang4core7builtin30output_2eflush__segment_7c3845(_env, seg, i) {
  const self = _env._1;
  const logger = _env._0;
  if (i > seg) {
    logger.method_table.method_1(logger.self, self, seg, i - seg | 0);
    return;
  } else {
    return;
  }
}
function _M0IP311moonbitlang4core6string6StringP311moonbitlang4core7builtin4Show6output(self, logger) {
  logger.method_table.method_3(logger.self, 34);
  const _env = { _0: logger, _1: self };
  const len = self.length;
  let _tmp = 0;
  let _tmp$2 = 0;
  _L: while (true) {
    const i = _tmp;
    const seg = _tmp$2;
    if (i >= len) {
      _M0FP311moonbitlang4core7builtin30output_2eflush__segment_7c3845(_env, seg, i);
      break;
    }
    const code = self.charCodeAt(i);
    let c;
    _L$2: {
      switch (code) {
        case 34: {
          c = code;
          break _L$2;
        }
        case 92: {
          c = code;
          break _L$2;
        }
        case 10: {
          _M0FP311moonbitlang4core7builtin30output_2eflush__segment_7c3845(_env, seg, i);
          logger.method_table.method_0(logger.self, "\\n");
          _tmp = i + 1 | 0;
          _tmp$2 = i + 1 | 0;
          continue _L;
        }
        case 13: {
          _M0FP311moonbitlang4core7builtin30output_2eflush__segment_7c3845(_env, seg, i);
          logger.method_table.method_0(logger.self, "\\r");
          _tmp = i + 1 | 0;
          _tmp$2 = i + 1 | 0;
          continue _L;
        }
        case 8: {
          _M0FP311moonbitlang4core7builtin30output_2eflush__segment_7c3845(_env, seg, i);
          logger.method_table.method_0(logger.self, "\\b");
          _tmp = i + 1 | 0;
          _tmp$2 = i + 1 | 0;
          continue _L;
        }
        case 9: {
          _M0FP311moonbitlang4core7builtin30output_2eflush__segment_7c3845(_env, seg, i);
          logger.method_table.method_0(logger.self, "\\t");
          _tmp = i + 1 | 0;
          _tmp$2 = i + 1 | 0;
          continue _L;
        }
        default: {
          if (_M0IP016_24default__implP311moonbitlang4core7builtin7Compare6op__ltGkE(code, 32)) {
            _M0FP311moonbitlang4core7builtin30output_2eflush__segment_7c3845(_env, seg, i);
            logger.method_table.method_0(logger.self, "\\u{");
            logger.method_table.method_0(logger.self, _M0MP311moonbitlang4core4byte4Byte7to__hex(code & 255));
            logger.method_table.method_3(logger.self, 125);
            _tmp = i + 1 | 0;
            _tmp$2 = i + 1 | 0;
            continue _L;
          } else {
            _tmp = i + 1 | 0;
            continue _L;
          }
        }
      }
    }
    _M0FP311moonbitlang4core7builtin30output_2eflush__segment_7c3845(_env, seg, i);
    logger.method_table.method_3(logger.self, 92);
    logger.method_table.method_3(logger.self, _M0MP311moonbitlang4core6uint166UInt1616unsafe__to__char(c));
    _tmp = i + 1 | 0;
    _tmp$2 = i + 1 | 0;
    continue;
  }
  logger.method_table.method_3(logger.self, 34);
}
function _M0IP311moonbitlang4core6string10StringViewP311moonbitlang4core7builtin4Show10to__string(self) {
  return self.str.substring(self.start, self.end);
}
function _M0IP311moonbitlang4core7builtin13StringBuilderP311moonbitlang4core7builtin6Logger11write__view(self, str) {
  const _bind = self;
  _bind.val = `${_bind.val}${_M0IP311moonbitlang4core6string10StringViewP311moonbitlang4core7builtin4Show10to__string(str)}`;
}
function _M0IP311moonbitlang4core3int3IntP311moonbitlang4core7builtin4Show6output(self, logger) {
  logger.method_table.method_0(logger.self, _M0MP311moonbitlang4core3int3Int18to__string_2einner(self, 10));
}
function _M0IP311moonbitlang4core6string6StringP311moonbitlang4core7builtin4Show10to__string(self) {
  return self;
}
function _M0FP311moonbitlang4core7builtin7printlnGsE(input) {
  console.log(_M0IP311moonbitlang4core6string6StringP311moonbitlang4core7builtin4Show10to__string(input));
}
function _M0IP016_24default__implP37astrata7astrata4main28MoonBit__Async__Test__Driver17run__async__testsGRP37astrata7astrata4main34MoonBit__Async__Test__Driver__ImplE(_discard_) {}
function _M0IP016_24default__implP37astrata7astrata4main21MoonBit__Test__Driver9run__testGRP37astrata7astrata4main41MoonBit__Test__Driver__Internal__No__ArgsE(_discard_, _discard_$2, _discard_$3, _discard_$4, _discard_$5) {
  return new Result$Ok$1$(false);
}
function _M0IP016_24default__implP37astrata7astrata4main21MoonBit__Test__Driver9run__testGRP37astrata7astrata4main43MoonBit__Test__Driver__Internal__With__ArgsE(_discard_, _discard_$2, _discard_$3, _discard_$4, _discard_$5) {
  return new Result$Ok$1$(false);
}
function _M0IP016_24default__implP37astrata7astrata4main21MoonBit__Test__Driver9run__testGRP37astrata7astrata4main48MoonBit__Test__Driver__Internal__Async__No__ArgsE(_discard_, _discard_$2, _discard_$3, _discard_$4, _discard_$5) {
  return new Result$Ok$1$(false);
}
function _M0IP016_24default__implP37astrata7astrata4main21MoonBit__Test__Driver9run__testGRP37astrata7astrata4main50MoonBit__Test__Driver__Internal__Async__With__ArgsE(_discard_, _discard_$2, _discard_$3, _discard_$4, _discard_$5) {
  return new Result$Ok$1$(false);
}
function _M0IP016_24default__implP37astrata7astrata4main21MoonBit__Test__Driver9run__testGRP37astrata7astrata4main50MoonBit__Test__Driver__Internal__With__Bench__ArgsE(_discard_, _discard_$2, _discard_$3, _discard_$4, _discard_$5) {
  return new Result$Ok$1$(false);
}
function _M0FP37astrata7astrata4main44moonbit__test__driver__internal__do__execute(async_tests, filename, index) {
  const handle_result = (testname, message, skipped) => {
    if (!skipped && false) {
    }
    const file_name = _M0MP311moonbitlang4core6string6String6escape(filename);
    const test_name = _M0MP311moonbitlang4core6string6String6escape(testname);
    const message$2 = _M0MP311moonbitlang4core6string6String6escape(message);
    _M0FP311moonbitlang4core7builtin7printlnGsE("----- BEGIN MOON TEST RESULT -----");
    _M0FP311moonbitlang4core7builtin7printlnGsE(`{\"package\": \"astrata/astrata/main\", \"filename\": ${_M0IP311moonbitlang4core6string6StringP311moonbitlang4core7builtin4Show10to__string(file_name)}, \"index\": \"${_M0IP016_24default__implP311moonbitlang4core7builtin4Show10to__stringGiE(index)}\", \"test_name\": ${_M0IP311moonbitlang4core6string6StringP311moonbitlang4core7builtin4Show10to__string(test_name)}, \"message\": ${_M0IP311moonbitlang4core6string6StringP311moonbitlang4core7builtin4Show10to__string(message$2)}}`);
    _M0FP311moonbitlang4core7builtin7printlnGsE("----- END MOON TEST RESULT -----");
  };
  const error_to_string = (err) => {
    let e;
    _L: {
      switch (err.$tag) {
        case 2: {
          const _Failure = err;
          const _e = _Failure._0;
          return _e;
        }
        case 4: {
          const _InspectError = err;
          const _e$2 = _InspectError._0;
          return _e$2;
        }
        case 5: {
          const _SnapshotError = err;
          const _e$3 = _SnapshotError._0;
          return _e$3;
        }
        case 6: {
          const _MoonBitTestDriverInternalJsError = err;
          const _e$4 = _MoonBitTestDriverInternalJsError._0;
          return _e$4;
        }
        default: {
          e = err;
          break _L;
        }
      }
    }
    return _M0FP15Error10to__string(e);
  };
  let _try_err;
  _L: {
    let _tmp;
    const _bind = _M0IP016_24default__implP37astrata7astrata4main21MoonBit__Test__Driver9run__testGRP37astrata7astrata4main41MoonBit__Test__Driver__Internal__No__ArgsE(async_tests, filename, index, handle_result, error_to_string);
    let _tmp$2;
    if (_bind.$tag === 1) {
      const _ok = _bind;
      _tmp$2 = _ok._0;
    } else {
      const _err = _bind;
      _try_err = _err._0;
      break _L;
    }
    if (_tmp$2) {
      _tmp = true;
    } else {
      let _tmp$3;
      const _bind$2 = _M0IP016_24default__implP37astrata7astrata4main21MoonBit__Test__Driver9run__testGRP37astrata7astrata4main43MoonBit__Test__Driver__Internal__With__ArgsE(async_tests, filename, index, handle_result, error_to_string);
      let _tmp$4;
      if (_bind$2.$tag === 1) {
        const _ok = _bind$2;
        _tmp$4 = _ok._0;
      } else {
        const _err = _bind$2;
        _try_err = _err._0;
        break _L;
      }
      if (_tmp$4) {
        _tmp$3 = true;
      } else {
        let _tmp$5;
        const _bind$3 = _M0IP016_24default__implP37astrata7astrata4main21MoonBit__Test__Driver9run__testGRP37astrata7astrata4main48MoonBit__Test__Driver__Internal__Async__No__ArgsE(async_tests, filename, index, handle_result, error_to_string);
        let _tmp$6;
        if (_bind$3.$tag === 1) {
          const _ok = _bind$3;
          _tmp$6 = _ok._0;
        } else {
          const _err = _bind$3;
          _try_err = _err._0;
          break _L;
        }
        if (_tmp$6) {
          _tmp$5 = true;
        } else {
          let _tmp$7;
          const _bind$4 = _M0IP016_24default__implP37astrata7astrata4main21MoonBit__Test__Driver9run__testGRP37astrata7astrata4main50MoonBit__Test__Driver__Internal__Async__With__ArgsE(async_tests, filename, index, handle_result, error_to_string);
          let _tmp$8;
          if (_bind$4.$tag === 1) {
            const _ok = _bind$4;
            _tmp$8 = _ok._0;
          } else {
            const _err = _bind$4;
            _try_err = _err._0;
            break _L;
          }
          if (_tmp$8) {
            _tmp$7 = true;
          } else {
            const _bind$5 = _M0IP016_24default__implP37astrata7astrata4main21MoonBit__Test__Driver9run__testGRP37astrata7astrata4main50MoonBit__Test__Driver__Internal__With__Bench__ArgsE(async_tests, filename, index, handle_result, error_to_string);
            let _tmp$9;
            if (_bind$5.$tag === 1) {
              const _ok = _bind$5;
              _tmp$9 = _ok._0;
            } else {
              const _err = _bind$5;
              _try_err = _err._0;
              break _L;
            }
            _tmp$7 = _tmp$9;
          }
          _tmp$5 = _tmp$7;
        }
        _tmp$3 = _tmp$5;
      }
      _tmp = _tmp$3;
    }
    if (!_tmp) {
      _try_err = new Error$astrata$47$astrata$47$main$46$MoonBitTestDriverInternalSkipTest$46$MoonBitTestDriverInternalSkipTest("");
      break _L;
    } else {
      return;
    }
  }
  const err = _try_err;
  let name;
  _L$2: {
    const _MoonBitTestDriverInternalSkipTest = err;
    const _name = _MoonBitTestDriverInternalSkipTest._0;
    name = _name;
    break _L$2;
  }
  handle_result(name, "skipped test", true);
}
function _M0FP37astrata7astrata4main40moonbit__test__driver__internal__execute(tests) {
  const async_tests = [];
  const _start22 = 0;
  const _end23 = tests.length;
  let _tmp = _start22;
  while (true) {
    const i = _tmp;
    if (i < _end23) {
      const entry = _M0MP311moonbitlang4core5array5Array2atGRP37astrata7astrata4main34MoonBitTestDriverInternalTestEntryE(tests, i);
      _M0FP37astrata7astrata4main44moonbit__test__driver__internal__do__execute(async_tests, _M0MP37astrata7astrata4main34MoonBitTestDriverInternalTestEntry8filename(entry), _M0MP37astrata7astrata4main34MoonBitTestDriverInternalTestEntry5index(entry));
      _tmp = i + 1 | 0;
      continue;
    } else {
      break;
    }
  }
  _M0IP016_24default__implP37astrata7astrata4main28MoonBit__Async__Test__Driver17run__async__testsGRP37astrata7astrata4main34MoonBit__Async__Test__Driver__ImplE(async_tests);
}
function _M0FP37astrata7astrata4main29moonbit__test__driver__finish() {}
(() => {
})();
exports.moonbit_test_driver_internal_execute = _M0FP37astrata7astrata4main40moonbit__test__driver__internal__execute;
exports.moonbit_test_driver_finish = _M0FP37astrata7astrata4main29moonbit__test__driver__finish;
//# sourceMappingURL=main.internal_test.js.map
