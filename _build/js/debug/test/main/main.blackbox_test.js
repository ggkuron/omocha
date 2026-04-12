const $reinterpret_view = new DataView(new ArrayBuffer(8));
function $f32_reinterpret_i32(a) {
  $reinterpret_view.setInt32(0, a, true);
  return $reinterpret_view.getFloat32(0, true);
}
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
function Error$astrata$47$astrata$47$main_blackbox_test$46$MoonBitTestDriverInternalJsError$46$MoonBitTestDriverInternalJsError(param0) {
  this._0 = param0;
}
Error$astrata$47$astrata$47$main_blackbox_test$46$MoonBitTestDriverInternalJsError$46$MoonBitTestDriverInternalJsError.prototype.$tag = 6;
Error$astrata$47$astrata$47$main_blackbox_test$46$MoonBitTestDriverInternalJsError$46$MoonBitTestDriverInternalJsError.prototype.$name = "astrata/astrata/main_blackbox_test.MoonBitTestDriverInternalJsError.MoonBitTestDriverInternalJsError";
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
function Error$astrata$47$astrata$47$main_blackbox_test$46$MoonBitTestDriverInternalSkipTest$46$MoonBitTestDriverInternalSkipTest(param0) {
  this._0 = param0;
}
Error$astrata$47$astrata$47$main_blackbox_test$46$MoonBitTestDriverInternalSkipTest$46$MoonBitTestDriverInternalSkipTest.prototype.$tag = 3;
Error$astrata$47$astrata$47$main_blackbox_test$46$MoonBitTestDriverInternalSkipTest$46$MoonBitTestDriverInternalSkipTest.prototype.$name = "astrata/astrata/main_blackbox_test.MoonBitTestDriverInternalSkipTest.MoonBitTestDriverInternalSkipTest";
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
const $64$astrata$47$astrata$47$lib$46$SatelliteSlot$Empty = { $tag: 0, $name: "Empty" };
function $64$astrata$47$astrata$47$lib$46$SatelliteSlot$Assigned(param0) {
  this._0 = param0;
}
$64$astrata$47$astrata$47$lib$46$SatelliteSlot$Assigned.prototype.$tag = 2;
$64$astrata$47$astrata$47$lib$46$SatelliteSlot$Assigned.prototype.$name = "Assigned";
function $64$astrata$47$astrata$47$lib$46$Selection$SideSelect(param0) {
  this._0 = param0;
}
$64$astrata$47$astrata$47$lib$46$Selection$SideSelect.prototype.$tag = 0;
$64$astrata$47$astrata$47$lib$46$Selection$SideSelect.prototype.$name = "SideSelect";
function $64$astrata$47$astrata$47$lib$46$Selection$FieldSelect(param0, param1) {
  this._0 = param0;
  this._1 = param1;
}
$64$astrata$47$astrata$47$lib$46$Selection$FieldSelect.prototype.$tag = 1;
$64$astrata$47$astrata$47$lib$46$Selection$FieldSelect.prototype.$name = "FieldSelect";
function $64$astrata$47$astrata$47$lib$46$Selection$SlotSelect(param0, param1, param2) {
  this._0 = param0;
  this._1 = param1;
  this._2 = param2;
}
$64$astrata$47$astrata$47$lib$46$Selection$SlotSelect.prototype.$tag = 2;
$64$astrata$47$astrata$47$lib$46$Selection$SlotSelect.prototype.$name = "SlotSelect";
function $64$astrata$47$astrata$47$lib$46$HandCard$HSatellite(param0) {
  this._0 = param0;
}
$64$astrata$47$astrata$47$lib$46$HandCard$HSatellite.prototype.$tag = 0;
$64$astrata$47$astrata$47$lib$46$HandCard$HSatellite.prototype.$name = "HSatellite";
function $64$astrata$47$astrata$47$lib$46$HandCard$HPortion(param0) {
  this._0 = param0;
}
$64$astrata$47$astrata$47$lib$46$HandCard$HPortion.prototype.$tag = 1;
$64$astrata$47$astrata$47$lib$46$HandCard$HPortion.prototype.$name = "HPortion";
function $64$astrata$47$astrata$47$lib$46$HandCard$HGuard(param0) {
  this._0 = param0;
}
$64$astrata$47$astrata$47$lib$46$HandCard$HGuard.prototype.$tag = 2;
$64$astrata$47$astrata$47$lib$46$HandCard$HGuard.prototype.$name = "HGuard";
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
function Result$Err$2$(param0) {
  this._0 = param0;
}
Result$Err$2$.prototype.$tag = 0;
Result$Err$2$.prototype.$name = "Err";
function Result$Ok$2$(param0) {
  this._0 = param0;
}
Result$Ok$2$.prototype.$tag = 1;
Result$Ok$2$.prototype.$name = "Ok";
function $64$mizchi$47$kagura$47$engine$47$gfx$46$BlendMode$Custom(param0) {
  this._0 = param0;
}
$64$mizchi$47$kagura$47$engine$47$gfx$46$BlendMode$Custom.prototype.$tag = 4;
$64$mizchi$47$kagura$47$engine$47$gfx$46$BlendMode$Custom.prototype.$name = "Custom";
const Option$None$3$ = { $tag: 0, $name: "None" };
function Option$Some$3$(param0) {
  this._0 = param0;
}
Option$Some$3$.prototype.$tag = 1;
Option$Some$3$.prototype.$name = "Some";
function Result$Err$4$(param0) {
  this._0 = param0;
}
Result$Err$4$.prototype.$tag = 0;
Result$Err$4$.prototype.$name = "Err";
function Result$Ok$4$(param0) {
  this._0 = param0;
}
Result$Ok$4$.prototype.$tag = 1;
Result$Ok$4$.prototype.$name = "Ok";
function Result$Err$5$(param0) {
  this._0 = param0;
}
Result$Err$5$.prototype.$tag = 0;
Result$Err$5$.prototype.$name = "Err";
function Result$Ok$5$(param0) {
  this._0 = param0;
}
Result$Ok$5$.prototype.$tag = 1;
Result$Ok$5$.prototype.$name = "Ok";
function $64$mizchi$47$signals$47$ui$46$Node$Element$6$(param0) {
  this._0 = param0;
}
$64$mizchi$47$signals$47$ui$46$Node$Element$6$.prototype.$tag = 0;
$64$mizchi$47$signals$47$ui$46$Node$Element$6$.prototype.$name = "Element";
function $64$mizchi$47$signals$47$ui$46$Node$Text$6$(param0) {
  this._0 = param0;
}
$64$mizchi$47$signals$47$ui$46$Node$Text$6$.prototype.$tag = 1;
$64$mizchi$47$signals$47$ui$46$Node$Text$6$.prototype.$name = "Text";
function $64$mizchi$47$signals$47$ui$46$Node$DynamicText$6$(param0) {
  this._0 = param0;
}
$64$mizchi$47$signals$47$ui$46$Node$DynamicText$6$.prototype.$tag = 2;
$64$mizchi$47$signals$47$ui$46$Node$DynamicText$6$.prototype.$name = "DynamicText";
function $64$mizchi$47$signals$47$ui$46$Node$Fragment$6$(param0) {
  this._0 = param0;
}
$64$mizchi$47$signals$47$ui$46$Node$Fragment$6$.prototype.$tag = 3;
$64$mizchi$47$signals$47$ui$46$Node$Fragment$6$.prototype.$name = "Fragment";
function $64$mizchi$47$signals$47$ui$46$Node$Show$6$(param0, param1) {
  this._0 = param0;
  this._1 = param1;
}
$64$mizchi$47$signals$47$ui$46$Node$Show$6$.prototype.$tag = 4;
$64$mizchi$47$signals$47$ui$46$Node$Show$6$.prototype.$name = "Show";
function $64$mizchi$47$signals$47$ui$46$Node$For$6$(param0) {
  this._0 = param0;
}
$64$mizchi$47$signals$47$ui$46$Node$For$6$.prototype.$tag = 5;
$64$mizchi$47$signals$47$ui$46$Node$For$6$.prototype.$name = "For";
function $64$mizchi$47$signals$47$ui$46$Node$Component$6$(param0) {
  this._0 = param0;
}
$64$mizchi$47$signals$47$ui$46$Node$Component$6$.prototype.$tag = 6;
$64$mizchi$47$signals$47$ui$46$Node$Component$6$.prototype.$name = "Component";
function $64$mizchi$47$signals$47$ui$46$Node$Async$6$(param0) {
  this._0 = param0;
}
$64$mizchi$47$signals$47$ui$46$Node$Async$6$.prototype.$tag = 7;
$64$mizchi$47$signals$47$ui$46$Node$Async$6$.prototype.$name = "Async";
function $64$mizchi$47$signals$47$ui$46$Node$ErrorBoundary$6$(param0) {
  this._0 = param0;
}
$64$mizchi$47$signals$47$ui$46$Node$ErrorBoundary$6$.prototype.$tag = 8;
$64$mizchi$47$signals$47$ui$46$Node$ErrorBoundary$6$.prototype.$name = "ErrorBoundary";
function $64$mizchi$47$signals$47$ui$46$Node$Switch$6$(param0) {
  this._0 = param0;
}
$64$mizchi$47$signals$47$ui$46$Node$Switch$6$.prototype.$tag = 9;
$64$mizchi$47$signals$47$ui$46$Node$Switch$6$.prototype.$name = "Switch";
function $64$mizchi$47$signals$47$ui$46$Node$RawHtml$6$(param0) {
  this._0 = param0;
}
$64$mizchi$47$signals$47$ui$46$Node$RawHtml$6$.prototype.$tag = 10;
$64$mizchi$47$signals$47$ui$46$Node$RawHtml$6$.prototype.$name = "RawHtml";
function $64$mizchi$47$signals$47$ui$46$Attr$VStatic$6$(param0) {
  this._0 = param0;
}
$64$mizchi$47$signals$47$ui$46$Attr$VStatic$6$.prototype.$tag = 0;
$64$mizchi$47$signals$47$ui$46$Attr$VStatic$6$.prototype.$name = "VStatic";
function $64$mizchi$47$signals$47$ui$46$Attr$VDynamic$6$(param0) {
  this._0 = param0;
}
$64$mizchi$47$signals$47$ui$46$Attr$VDynamic$6$.prototype.$tag = 1;
$64$mizchi$47$signals$47$ui$46$Attr$VDynamic$6$.prototype.$name = "VDynamic";
function $64$mizchi$47$signals$47$ui$46$Attr$VHandler$6$(param0) {
  this._0 = param0;
}
$64$mizchi$47$signals$47$ui$46$Attr$VHandler$6$.prototype.$tag = 2;
$64$mizchi$47$signals$47$ui$46$Attr$VHandler$6$.prototype.$name = "VHandler";
function $64$mizchi$47$kagura$47$scene$46$SceneAttrValue$SceneInt(param0) {
  this._0 = param0;
}
$64$mizchi$47$kagura$47$scene$46$SceneAttrValue$SceneInt.prototype.$tag = 0;
$64$mizchi$47$kagura$47$scene$46$SceneAttrValue$SceneInt.prototype.$name = "SceneInt";
function $64$mizchi$47$kagura$47$scene$46$SceneAttrValue$SceneDouble(param0) {
  this._0 = param0;
}
$64$mizchi$47$kagura$47$scene$46$SceneAttrValue$SceneDouble.prototype.$tag = 1;
$64$mizchi$47$kagura$47$scene$46$SceneAttrValue$SceneDouble.prototype.$name = "SceneDouble";
function $64$mizchi$47$kagura$47$scene$46$SceneAttrValue$SceneText(param0) {
  this._0 = param0;
}
$64$mizchi$47$kagura$47$scene$46$SceneAttrValue$SceneText.prototype.$tag = 2;
$64$mizchi$47$kagura$47$scene$46$SceneAttrValue$SceneText.prototype.$name = "SceneText";
function Option$Some$7$(param0) {
  this._0 = param0;
}
Option$Some$7$.prototype.$tag = 1;
Option$Some$7$.prototype.$name = "Some";
function Option$Some$8$(param0) {
  this._0 = param0;
}
Option$Some$8$.prototype.$tag = 1;
Option$Some$8$.prototype.$name = "Some";
function Result$Err$9$(param0) {
  this._0 = param0;
}
Result$Err$9$.prototype.$tag = 0;
Result$Err$9$.prototype.$name = "Err";
function Result$Ok$9$(param0) {
  this._0 = param0;
}
Result$Ok$9$.prototype.$tag = 1;
Result$Ok$9$.prototype.$name = "Ok";
const _M0MP37astrata7astrata20main__blackbox__test34MoonBitTestDriverInternalTestEntry8filename = (entry) => entry[0];
const _M0MP37astrata7astrata20main__blackbox__test34MoonBitTestDriverInternalTestEntry5index = (entry) => entry[1];
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
    case 3: {
      return "astrata/astrata/main_blackbox_test.MoonBitTestDriverInternalSkipTest.MoonBitTestDriverInternalSkipTest";
    }
    case 6: {
      return "astrata/astrata/main_blackbox_test.MoonBitTestDriverInternalJsError.MoonBitTestDriverInternalJsError";
    }
    case 2: {
      return _M0IP016_24default__implP311moonbitlang4core7builtin4Show10to__stringGRP311moonbitlang4core7builtin7FailureE(_e);
    }
    default: {
      return "moonbitlang/core/builtin.InspectError.InspectError";
    }
  }
}
$f32_reinterpret_i32(-8388609);
$f32_reinterpret_i32(2139095039);
$f32_reinterpret_i32(2143289344);
const _M0FP37astrata7astrata3lib19card__satellite__s0 = { prop: { name: "Satellite-S0", cost: 1 }, battler: { atk: 1, hp: 2 } };
const _M0FP37astrata7astrata3lib19card__satellite__m0 = { prop: { name: "Satellite-M0", cost: 2 }, battler: { atk: 2, hp: 3 } };
const _M0FP37astrata7astrata3lib18card__origin__base = { prop: { name: "Origin", cost: 0 }, max_hp: 6 };
_M0MP311moonbitlang4core3ref3Ref3newGRP46mizchi6kagura6engine8platform14WebCanvasHooksE(_M0FP46mizchi6kagura6engine8platform27default__web__canvas__hooks());
_M0MP311moonbitlang4core3ref3Ref3newGORP36mizchi6kagura6engine14LifecycleHooksE(undefined);
_M0MP311moonbitlang4core3ref3Ref3newGRP46mizchi6kagura6engine3gfx19NativeGraphicsHooksE(_M0FP46mizchi6kagura6engine3gfx32default__native__graphics__hooks());
_M0MP311moonbitlang4core3ref3Ref3newGRP46mizchi6kagura6engine5audio16AudioOutputHooksE(_M0FP46mizchi6kagura6engine5audio29default__audio__output__hooks());
_M0MP311moonbitlang4core3ref3Ref3newGRP46mizchi6kagura6engine3gfx16WebGraphicsHooksE(_M0FP46mizchi6kagura6engine3gfx29default__web__graphics__hooks());
_M0MP311moonbitlang4core3ref3Ref3newGWEdE(_M0FP46mizchi6kagura6engine3gfx23default__clock__now__ms);
({ own: _M0FP37astrata7astrata3lib16make__own__field(), rival: _M0FP37astrata7astrata3lib18make__rival__field() });
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
function _M0MP311moonbitlang4core5array5Array2atGRP37astrata7astrata20main__blackbox__test34MoonBitTestDriverInternalTestEntryE(self, index) {
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
function _M0MP311moonbitlang4core3ref3Ref3newGORP36mizchi6kagura6engine14LifecycleHooksE(x) {
  return { val: x };
}
function _M0MP311moonbitlang4core3ref3Ref3newGRP46mizchi6kagura6engine5audio16AudioOutputHooksE(x) {
  return { val: x };
}
function _M0MP311moonbitlang4core3ref3Ref3newGWEdE(x) {
  return { val: x };
}
function _M0MP311moonbitlang4core3ref3Ref3newGRP46mizchi6kagura6engine8platform14WebCanvasHooksE(x) {
  return { val: x };
}
function _M0MP311moonbitlang4core3ref3Ref3newGRP46mizchi6kagura6engine3gfx16WebGraphicsHooksE(x) {
  return { val: x };
}
function _M0MP311moonbitlang4core3ref3Ref3newGRP46mizchi6kagura6engine3gfx19NativeGraphicsHooksE(x) {
  return { val: x };
}
function _M0FP37astrata7astrata3lib12make__origin(id) {
  return { id: id, card: _M0FP37astrata7astrata3lib18card__origin__base, state: { current_hp: _M0FP37astrata7astrata3lib18card__origin__base.max_hp } };
}
function _M0FP37astrata7astrata3lib15make__satellite(id, card) {
  return { id: id, card: card, battle: { tired: false, current_hp: card.battler.hp } };
}
function _M0FP37astrata7astrata3lib16make__own__field() {
  const hands = [new $64$astrata$47$astrata$47$lib$46$HandCard$HSatellite(_M0FP37astrata7astrata3lib15make__satellite(101, _M0FP37astrata7astrata3lib19card__satellite__s0)), new $64$astrata$47$astrata$47$lib$46$HandCard$HSatellite(_M0FP37astrata7astrata3lib15make__satellite(102, _M0FP37astrata7astrata3lib19card__satellite__s0)), new $64$astrata$47$astrata$47$lib$46$HandCard$HSatellite(_M0FP37astrata7astrata3lib15make__satellite(103, _M0FP37astrata7astrata3lib19card__satellite__s0)), new $64$astrata$47$astrata$47$lib$46$HandCard$HSatellite(_M0FP37astrata7astrata3lib15make__satellite(104, _M0FP37astrata7astrata3lib19card__satellite__m0)), new $64$astrata$47$astrata$47$lib$46$HandCard$HSatellite(_M0FP37astrata7astrata3lib15make__satellite(105, _M0FP37astrata7astrata3lib19card__satellite__m0))];
  return { origin: _M0FP37astrata7astrata3lib12make__origin(100), front_l: $64$astrata$47$astrata$47$lib$46$SatelliteSlot$Empty, front_r: $64$astrata$47$astrata$47$lib$46$SatelliteSlot$Empty, back_l: $64$astrata$47$astrata$47$lib$46$SatelliteSlot$Empty, back_r: $64$astrata$47$astrata$47$lib$46$SatelliteSlot$Empty, aura: [], astra_records: [], hands: hands, stack: [] };
}
function _M0FP37astrata7astrata3lib18make__rival__field() {
  const hands = [new $64$astrata$47$astrata$47$lib$46$HandCard$HSatellite(_M0FP37astrata7astrata3lib15make__satellite(201, _M0FP37astrata7astrata3lib19card__satellite__s0)), new $64$astrata$47$astrata$47$lib$46$HandCard$HSatellite(_M0FP37astrata7astrata3lib15make__satellite(202, _M0FP37astrata7astrata3lib19card__satellite__s0)), new $64$astrata$47$astrata$47$lib$46$HandCard$HSatellite(_M0FP37astrata7astrata3lib15make__satellite(203, _M0FP37astrata7astrata3lib19card__satellite__m0)), new $64$astrata$47$astrata$47$lib$46$HandCard$HSatellite(_M0FP37astrata7astrata3lib15make__satellite(204, _M0FP37astrata7astrata3lib19card__satellite__m0)), new $64$astrata$47$astrata$47$lib$46$HandCard$HSatellite(_M0FP37astrata7astrata3lib15make__satellite(205, _M0FP37astrata7astrata3lib19card__satellite__s0))];
  return { origin: _M0FP37astrata7astrata3lib12make__origin(200), front_l: new $64$astrata$47$astrata$47$lib$46$SatelliteSlot$Assigned(_M0FP37astrata7astrata3lib15make__satellite(206, _M0FP37astrata7astrata3lib19card__satellite__s0)), front_r: new $64$astrata$47$astrata$47$lib$46$SatelliteSlot$Assigned(_M0FP37astrata7astrata3lib15make__satellite(207, _M0FP37astrata7astrata3lib19card__satellite__s0)), back_l: new $64$astrata$47$astrata$47$lib$46$SatelliteSlot$Assigned(_M0FP37astrata7astrata3lib15make__satellite(208, _M0FP37astrata7astrata3lib19card__satellite__m0)), back_r: $64$astrata$47$astrata$47$lib$46$SatelliteSlot$Empty, aura: [], astra_records: [], hands: hands, stack: [] };
}
function _M0FP36mizchi6kagura4core18new__outside__size(width, height) {
  return { width: width, height: height };
}
function _M0FP36mizchi6kagura4core26new__input__snapshot__full(cursor_x, cursor_y, wheel_x, wheel_y, pressed_keys, pressed_mouse_buttons, touches, gamepads) {
  return { cursor_x: cursor_x, cursor_y: cursor_y, wheel_x: wheel_x, wheel_y: wheel_y, pressed_keys: pressed_keys, pressed_mouse_buttons: pressed_mouse_buttons, touches: touches, gamepads: gamepads };
}
function _M0FP36mizchi6kagura4core20new__input__snapshot(cursor_x, cursor_y, wheel_x, wheel_y, pressed_keys) {
  return _M0FP36mizchi6kagura4core26new__input__snapshot__full(cursor_x, cursor_y, wheel_x, wheel_y, pressed_keys, [], [], []);
}
function _M0FP36mizchi6kagura4core22empty__input__snapshot() {
  return _M0FP36mizchi6kagura4core20new__input__snapshot(0, 0, 0, 0, []);
}
function _M0FP46mizchi6kagura6engine5audio31default__audio__output__latency() {
  return 0;
}
function _M0FP46mizchi6kagura6engine5audio25new__audio__output__hooks(try_initialize, write_frames, suspend, resume_playback, close) {
  return { try_initialize: try_initialize, write_frames: write_frames, suspend: suspend, resume_playback: resume_playback, close: close, output_latency: _M0FP46mizchi6kagura6engine5audio31default__audio__output__latency };
}
function _M0FP46mizchi6kagura6engine5audio31default__audio__try__initialize(_format) {
  return false;
}
function _M0FP46mizchi6kagura6engine5audio29default__audio__write__frames(_output, _frames) {
  return 0;
}
function _M0FP46mizchi6kagura6engine5audio23default__audio__suspend() {}
function _M0FP46mizchi6kagura6engine5audio22default__audio__resume() {}
function _M0FP46mizchi6kagura6engine5audio21default__audio__close() {}
function _M0FP46mizchi6kagura6engine5audio29default__audio__output__hooks() {
  return _M0FP46mizchi6kagura6engine5audio25new__audio__output__hooks(_M0FP46mizchi6kagura6engine5audio31default__audio__try__initialize, _M0FP46mizchi6kagura6engine5audio29default__audio__write__frames, _M0FP46mizchi6kagura6engine5audio23default__audio__suspend, _M0FP46mizchi6kagura6engine5audio22default__audio__resume, _M0FP46mizchi6kagura6engine5audio21default__audio__close);
}
function _M0FP46mizchi6kagura6engine8platform23new__web__canvas__hooks(try_initialize, poll, should_close, outside_size, current_surface, capture_input, set_fullscreen, is_fullscreen, set_cursor_mode, cursor_mode, set_device_scale_factor, device_scale_factor, set_vsync_enabled, is_vsync_enabled, close_window, request_attention, set_mouse_touch_fallback, mouse_touch_fallback_enabled, fullscreen_request_status, cursor_mode_request_status) {
  return { try_initialize: try_initialize, poll: poll, should_close: should_close, outside_size: outside_size, current_surface: current_surface, capture_input: capture_input, set_fullscreen: set_fullscreen, is_fullscreen: is_fullscreen, set_cursor_mode: set_cursor_mode, cursor_mode: cursor_mode, set_device_scale_factor: set_device_scale_factor, device_scale_factor: device_scale_factor, set_vsync_enabled: set_vsync_enabled, is_vsync_enabled: is_vsync_enabled, close_window: close_window, request_attention: request_attention, set_mouse_touch_fallback: set_mouse_touch_fallback, mouse_touch_fallback_enabled: mouse_touch_fallback_enabled, fullscreen_request_status: fullscreen_request_status, cursor_mode_request_status: cursor_mode_request_status };
}
function _M0FP46mizchi6kagura6engine8platform29default__web__try__initialize(_canvas_selector, _options) {
  return false;
}
function _M0FP46mizchi6kagura6engine8platform18default__web__poll(_active) {}
function _M0FP46mizchi6kagura6engine8platform27default__web__should__close() {
  return false;
}
function _M0FP46mizchi6kagura6engine8platform27default__web__outside__size(width, height) {
  return _M0FP36mizchi6kagura4core18new__outside__size(width + 0, height + 0);
}
function _M0FP46mizchi6kagura6engine8platform30default__web__current__surface(_canvas_selector, options) {
  return { kind: 1, opaque_id: 2, width: options.width, height: options.height, device_scale_factor: 1 };
}
function _M0FP46mizchi6kagura6engine8platform28default__web__capture__input(_active, _tick) {
  return _M0FP36mizchi6kagura4core22empty__input__snapshot();
}
function _M0FP46mizchi6kagura6engine8platform29default__web__set__fullscreen(_canvas_selector, _active, enabled) {
  return enabled;
}
function _M0FP46mizchi6kagura6engine8platform28default__web__is__fullscreen(_canvas_selector, _active, current) {
  return current;
}
function _M0FP46mizchi6kagura6engine8platform31default__web__set__cursor__mode(_canvas_selector, _active, mode) {
  return mode;
}
function _M0FP46mizchi6kagura6engine8platform26default__web__cursor__mode(_canvas_selector, _active, current) {
  return current;
}
function _M0FP46mizchi6kagura6engine8platform40default__web__set__device__scale__factor(_canvas_selector, _active, scale) {
  return scale <= 0 ? 1 : scale;
}
function _M0FP46mizchi6kagura6engine8platform35default__web__device__scale__factor(_canvas_selector, _active, current) {
  return current <= 0 ? 1 : current;
}
function _M0FP46mizchi6kagura6engine8platform33default__web__set__vsync__enabled(_canvas_selector, _active, enabled) {
  return enabled;
}
function _M0FP46mizchi6kagura6engine8platform32default__web__is__vsync__enabled(_canvas_selector, _active, current) {
  return current;
}
function _M0FP46mizchi6kagura6engine8platform27default__web__close__window(_canvas_selector, _active) {}
function _M0FP46mizchi6kagura6engine8platform32default__web__request__attention(_canvas_selector, _active) {}
function _M0FP46mizchi6kagura6engine8platform41default__web__set__mouse__touch__fallback(_canvas_selector, _active, _enabled) {}
function _M0FP46mizchi6kagura6engine8platform45default__web__mouse__touch__fallback__enabled(_canvas_selector, _active) {
  return false;
}
function _M0FP46mizchi6kagura6engine8platform41default__web__fullscreen__request__status(_canvas_selector, _active, current) {
  return current;
}
function _M0FP46mizchi6kagura6engine8platform43default__web__cursor__mode__request__status(_canvas_selector, _active, current) {
  return current;
}
function _M0FP46mizchi6kagura6engine8platform27default__web__canvas__hooks() {
  return _M0FP46mizchi6kagura6engine8platform23new__web__canvas__hooks(_M0FP46mizchi6kagura6engine8platform29default__web__try__initialize, _M0FP46mizchi6kagura6engine8platform18default__web__poll, _M0FP46mizchi6kagura6engine8platform27default__web__should__close, _M0FP46mizchi6kagura6engine8platform27default__web__outside__size, _M0FP46mizchi6kagura6engine8platform30default__web__current__surface, _M0FP46mizchi6kagura6engine8platform28default__web__capture__input, _M0FP46mizchi6kagura6engine8platform29default__web__set__fullscreen, _M0FP46mizchi6kagura6engine8platform28default__web__is__fullscreen, _M0FP46mizchi6kagura6engine8platform31default__web__set__cursor__mode, _M0FP46mizchi6kagura6engine8platform26default__web__cursor__mode, _M0FP46mizchi6kagura6engine8platform40default__web__set__device__scale__factor, _M0FP46mizchi6kagura6engine8platform35default__web__device__scale__factor, _M0FP46mizchi6kagura6engine8platform33default__web__set__vsync__enabled, _M0FP46mizchi6kagura6engine8platform32default__web__is__vsync__enabled, _M0FP46mizchi6kagura6engine8platform27default__web__close__window, _M0FP46mizchi6kagura6engine8platform32default__web__request__attention, _M0FP46mizchi6kagura6engine8platform41default__web__set__mouse__touch__fallback, _M0FP46mizchi6kagura6engine8platform45default__web__mouse__touch__fallback__enabled, _M0FP46mizchi6kagura6engine8platform41default__web__fullscreen__request__status, _M0FP46mizchi6kagura6engine8platform43default__web__cursor__mode__request__status);
}
function _M0FP46mizchi6kagura6engine3gfx30default__web__on__read__pixels(_active, _kind, _x, _y, _width, _height) {
  return Option$None$3$;
}
function _M0FP46mizchi6kagura6engine3gfx25new__web__graphics__hooks(try_initialize, on_begin, on_end, on_draw, on_resize) {
  return { try_initialize: try_initialize, on_begin: on_begin, on_end: on_end, on_draw: on_draw, on_resize: on_resize, on_read_pixels: _M0FP46mizchi6kagura6engine3gfx30default__web__on__read__pixels };
}
function _M0FP46mizchi6kagura6engine3gfx29default__web__try__initialize(_kind, _width, _height) {
  return false;
}
function _M0FP46mizchi6kagura6engine3gfx23default__web__on__begin(_active, _kind, _pass) {}
function _M0FP46mizchi6kagura6engine3gfx21default__web__on__end(_active, _kind, _present) {}
function _M0FP46mizchi6kagura6engine3gfx22default__web__on__draw(_active, _kind, _command) {}
function _M0FP46mizchi6kagura6engine3gfx24default__web__on__resize(_active, _kind, _width, _height) {}
function _M0FP46mizchi6kagura6engine3gfx29default__web__graphics__hooks() {
  return _M0FP46mizchi6kagura6engine3gfx25new__web__graphics__hooks(_M0FP46mizchi6kagura6engine3gfx29default__web__try__initialize, _M0FP46mizchi6kagura6engine3gfx23default__web__on__begin, _M0FP46mizchi6kagura6engine3gfx21default__web__on__end, _M0FP46mizchi6kagura6engine3gfx22default__web__on__draw, _M0FP46mizchi6kagura6engine3gfx24default__web__on__resize);
}
function _M0FP46mizchi6kagura6engine3gfx31default__native__on__new__image(_active, _image_id, _width, _height) {}
function _M0FP46mizchi6kagura6engine3gfx33default__native__on__read__pixels(_active, _x, _y, _width, _height) {
  return Option$None$3$;
}
function _M0FP46mizchi6kagura6engine3gfx28new__native__graphics__hooks(try_initialize, on_begin, on_end, on_draw, on_resize) {
  return { try_initialize: try_initialize, on_begin: on_begin, on_end: on_end, on_draw: on_draw, on_resize: on_resize, on_read_pixels: _M0FP46mizchi6kagura6engine3gfx33default__native__on__read__pixels, on_new_image: _M0FP46mizchi6kagura6engine3gfx31default__native__on__new__image };
}
function _M0FP46mizchi6kagura6engine3gfx32default__native__try__initialize(_width, _height) {
  return false;
}
function _M0FP46mizchi6kagura6engine3gfx26default__native__on__begin(_active, _pass) {}
function _M0FP46mizchi6kagura6engine3gfx24default__native__on__end(_active, _present) {}
function _M0FP46mizchi6kagura6engine3gfx25default__native__on__draw(_active, _command) {}
function _M0FP46mizchi6kagura6engine3gfx27default__native__on__resize(_active, _width, _height) {}
function _M0FP46mizchi6kagura6engine3gfx32default__native__graphics__hooks() {
  return _M0FP46mizchi6kagura6engine3gfx28new__native__graphics__hooks(_M0FP46mizchi6kagura6engine3gfx32default__native__try__initialize, _M0FP46mizchi6kagura6engine3gfx26default__native__on__begin, _M0FP46mizchi6kagura6engine3gfx24default__native__on__end, _M0FP46mizchi6kagura6engine3gfx25default__native__on__draw, _M0FP46mizchi6kagura6engine3gfx27default__native__on__resize);
}
function _M0FP46mizchi6kagura6engine3gfx23default__clock__now__ms() {
  return 0;
}
function _M0IP016_24default__implP37astrata7astrata20main__blackbox__test28MoonBit__Async__Test__Driver17run__async__testsGRP37astrata7astrata20main__blackbox__test34MoonBit__Async__Test__Driver__ImplE(_discard_) {}
function _M0IP016_24default__implP37astrata7astrata20main__blackbox__test21MoonBit__Test__Driver9run__testGRP37astrata7astrata20main__blackbox__test41MoonBit__Test__Driver__Internal__No__ArgsE(_discard_, _discard_$2, _discard_$3, _discard_$4, _discard_$5) {
  return new Result$Ok$9$(false);
}
function _M0IP016_24default__implP37astrata7astrata20main__blackbox__test21MoonBit__Test__Driver9run__testGRP37astrata7astrata20main__blackbox__test43MoonBit__Test__Driver__Internal__With__ArgsE(_discard_, _discard_$2, _discard_$3, _discard_$4, _discard_$5) {
  return new Result$Ok$9$(false);
}
function _M0IP016_24default__implP37astrata7astrata20main__blackbox__test21MoonBit__Test__Driver9run__testGRP37astrata7astrata20main__blackbox__test48MoonBit__Test__Driver__Internal__Async__No__ArgsE(_discard_, _discard_$2, _discard_$3, _discard_$4, _discard_$5) {
  return new Result$Ok$9$(false);
}
function _M0IP016_24default__implP37astrata7astrata20main__blackbox__test21MoonBit__Test__Driver9run__testGRP37astrata7astrata20main__blackbox__test50MoonBit__Test__Driver__Internal__Async__With__ArgsE(_discard_, _discard_$2, _discard_$3, _discard_$4, _discard_$5) {
  return new Result$Ok$9$(false);
}
function _M0IP016_24default__implP37astrata7astrata20main__blackbox__test21MoonBit__Test__Driver9run__testGRP37astrata7astrata20main__blackbox__test50MoonBit__Test__Driver__Internal__With__Bench__ArgsE(_discard_, _discard_$2, _discard_$3, _discard_$4, _discard_$5) {
  return new Result$Ok$9$(false);
}
function _M0FP37astrata7astrata20main__blackbox__test44moonbit__test__driver__internal__do__execute(async_tests, filename, index) {
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
    const _bind = _M0IP016_24default__implP37astrata7astrata20main__blackbox__test21MoonBit__Test__Driver9run__testGRP37astrata7astrata20main__blackbox__test41MoonBit__Test__Driver__Internal__No__ArgsE(async_tests, filename, index, handle_result, error_to_string);
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
      const _bind$2 = _M0IP016_24default__implP37astrata7astrata20main__blackbox__test21MoonBit__Test__Driver9run__testGRP37astrata7astrata20main__blackbox__test43MoonBit__Test__Driver__Internal__With__ArgsE(async_tests, filename, index, handle_result, error_to_string);
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
        const _bind$3 = _M0IP016_24default__implP37astrata7astrata20main__blackbox__test21MoonBit__Test__Driver9run__testGRP37astrata7astrata20main__blackbox__test48MoonBit__Test__Driver__Internal__Async__No__ArgsE(async_tests, filename, index, handle_result, error_to_string);
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
          const _bind$4 = _M0IP016_24default__implP37astrata7astrata20main__blackbox__test21MoonBit__Test__Driver9run__testGRP37astrata7astrata20main__blackbox__test50MoonBit__Test__Driver__Internal__Async__With__ArgsE(async_tests, filename, index, handle_result, error_to_string);
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
            const _bind$5 = _M0IP016_24default__implP37astrata7astrata20main__blackbox__test21MoonBit__Test__Driver9run__testGRP37astrata7astrata20main__blackbox__test50MoonBit__Test__Driver__Internal__With__Bench__ArgsE(async_tests, filename, index, handle_result, error_to_string);
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
      _try_err = new Error$astrata$47$astrata$47$main_blackbox_test$46$MoonBitTestDriverInternalSkipTest$46$MoonBitTestDriverInternalSkipTest("");
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
function _M0FP37astrata7astrata20main__blackbox__test40moonbit__test__driver__internal__execute(tests) {
  const async_tests = [];
  const _start22 = 0;
  const _end23 = tests.length;
  let _tmp = _start22;
  while (true) {
    const i = _tmp;
    if (i < _end23) {
      const entry = _M0MP311moonbitlang4core5array5Array2atGRP37astrata7astrata20main__blackbox__test34MoonBitTestDriverInternalTestEntryE(tests, i);
      _M0FP37astrata7astrata20main__blackbox__test44moonbit__test__driver__internal__do__execute(async_tests, _M0MP37astrata7astrata20main__blackbox__test34MoonBitTestDriverInternalTestEntry8filename(entry), _M0MP37astrata7astrata20main__blackbox__test34MoonBitTestDriverInternalTestEntry5index(entry));
      _tmp = i + 1 | 0;
      continue;
    } else {
      break;
    }
  }
  _M0IP016_24default__implP37astrata7astrata20main__blackbox__test28MoonBit__Async__Test__Driver17run__async__testsGRP37astrata7astrata20main__blackbox__test34MoonBit__Async__Test__Driver__ImplE(async_tests);
}
function _M0FP37astrata7astrata20main__blackbox__test29moonbit__test__driver__finish() {}
(() => {
})();
exports.moonbit_test_driver_internal_execute = _M0FP37astrata7astrata20main__blackbox__test40moonbit__test__driver__internal__execute;
exports.moonbit_test_driver_finish = _M0FP37astrata7astrata20main__blackbox__test29moonbit__test__driver__finish;
//# sourceMappingURL=main.blackbox_test.js.map
