class $PanicError extends Error {}
function $panic() {
  throw new $PanicError();
}
function $bound_check(arr, index) {
  if (index < 0 || index >= arr.length) throw new Error("Index out of bounds");
}
function $compare_int(a, b) {
  return (a >= b) - (a <= b);
}
const _M0FP311moonbitlang4core7builtin12random__seed = () => {
  if (globalThis.crypto?.getRandomValues) {
    const array = new Uint32Array(1);
    globalThis.crypto.getRandomValues(array);
    return array[0] | 0; // Convert to signed 32
  } else {
    return Math.floor(Math.random() * 0x100000000) | 0; // Fallback to Math.random
  }
};
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
function Error$moonbitlang$47$core$47$builtin$46$SnapshotError$46$SnapshotError(param0) {
  this._0 = param0;
}
Error$moonbitlang$47$core$47$builtin$46$SnapshotError$46$SnapshotError.prototype.$tag = 6;
Error$moonbitlang$47$core$47$builtin$46$SnapshotError$46$SnapshotError.prototype.$name = "moonbitlang/core/builtin.SnapshotError.SnapshotError";
function Error$moonbitlang$47$core$47$builtin$46$InspectError$46$InspectError(param0) {
  this._0 = param0;
}
Error$moonbitlang$47$core$47$builtin$46$InspectError$46$InspectError.prototype.$tag = 5;
Error$moonbitlang$47$core$47$builtin$46$InspectError$46$InspectError.prototype.$name = "moonbitlang/core/builtin.InspectError.InspectError";
function Error$astrata$47$astrata$47$main$46$MoonBitTestDriverInternalJsError$46$MoonBitTestDriverInternalJsError(param0) {
  this._0 = param0;
}
Error$astrata$47$astrata$47$main$46$MoonBitTestDriverInternalJsError$46$MoonBitTestDriverInternalJsError.prototype.$tag = 4;
Error$astrata$47$astrata$47$main$46$MoonBitTestDriverInternalJsError$46$MoonBitTestDriverInternalJsError.prototype.$name = "astrata/astrata/main.MoonBitTestDriverInternalJsError.MoonBitTestDriverInternalJsError";
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
const _M0MP311moonbitlang4core7builtin7JSArray4push = (arr, val) => { arr.push(val); };
function $make_array_len_and_init(a, b) {
  const arr = new Array(a);
  arr.fill(b);
  return arr;
}
const _M0MP311moonbitlang4core7builtin7JSArray4copy = (arr) => arr.slice(0);
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
function Result$Err$3$(param0) {
  this._0 = param0;
}
Result$Err$3$.prototype.$tag = 0;
Result$Err$3$.prototype.$name = "Err";
function Result$Ok$3$(param0) {
  this._0 = param0;
}
Result$Ok$3$.prototype.$tag = 1;
Result$Ok$3$.prototype.$name = "Ok";
const _M0MP37astrata7astrata4main34MoonBitTestDriverInternalTestEntry8filename = (entry) => entry[0];
const _M0MP37astrata7astrata4main34MoonBitTestDriverInternalTestEntry5index = (entry) => entry[1];
const _M0FP37astrata7astrata4main42moonbit__test__driver__internal__js__catch = (f, on_err) => {
   try {
     f()
   } catch (err) {
     const msg = err.stack.toString()
     on_err(msg)
   }
 };
const _M0FP095_40moonbitlang_2fcore_2fbuiltin_2eStringBuilder_24as_24_40moonbitlang_2fcore_2fbuiltin_2eLogger = { method_0: _M0IP311moonbitlang4core7builtin13StringBuilderP311moonbitlang4core7builtin6Logger13write__string, method_1: _M0IP016_24default__implP311moonbitlang4core7builtin6Logger16write__substringGRP311moonbitlang4core7builtin13StringBuilderE, method_2: _M0IP311moonbitlang4core7builtin13StringBuilderP311moonbitlang4core7builtin6Logger11write__view, method_3: _M0IP311moonbitlang4core7builtin13StringBuilderP311moonbitlang4core7builtin6Logger11write__char };
function _M0FP15Error10to__string(_e) {
  switch (_e.$tag) {
    case 0: {
      return _M0IP016_24default__implP311moonbitlang4core7builtin4Show10to__stringGRP311moonbitlang4core7builtin17CreatingViewErrorE(_e);
    }
    case 1: {
      return _M0IP016_24default__implP311moonbitlang4core7builtin4Show10to__stringGRP311moonbitlang4core7builtin17CreatingViewErrorE(_e);
    }
    case 6: {
      return "moonbitlang/core/builtin.SnapshotError.SnapshotError";
    }
    case 4: {
      return "astrata/astrata/main.MoonBitTestDriverInternalJsError.MoonBitTestDriverInternalJsError";
    }
    case 2: {
      return _M0IP016_24default__implP311moonbitlang4core7builtin4Show10to__stringGRP311moonbitlang4core7builtin7FailureE(_e);
    }
    case 5: {
      return "moonbitlang/core/builtin.InspectError.InspectError";
    }
    default: {
      return "astrata/astrata/main.MoonBitTestDriverInternalSkipTest.MoonBitTestDriverInternalSkipTest";
    }
  }
}
const _M0FP37astrata7astrata3lib19card__satellite__s0 = { prop: { name: "Satellite-S0", cost: 1 }, battler: { atk: 1, hp: 2 } };
const _M0FP37astrata7astrata3lib19card__satellite__m0 = { prop: { name: "Satellite-M0", cost: 2 }, battler: { atk: 2, hp: 3 } };
const _M0FP37astrata7astrata3lib18card__origin__base = { prop: { name: "Origin", cost: 0 }, max_hp: 6 };
const _M0FP46mizchi6kagura4core9inpututil6key__w = 87;
const _M0FP46mizchi6kagura4core9inpututil6key__s = 83;
const _M0FP46mizchi6kagura4core9inpututil12key__up__web = 38;
const _M0FP46mizchi6kagura4core9inpututil14key__down__web = 40;
const _M0FP46mizchi6kagura4core9inpututil14key__left__web = 37;
const _M0FP46mizchi6kagura4core9inpututil15key__right__web = 39;
const _M0FP46mizchi6kagura4core9inpututil13key__up__glfw = 265;
const _M0FP46mizchi6kagura4core9inpututil15key__down__glfw = 264;
const _M0FP46mizchi6kagura4core9inpututil10key__comma = 188;
const _M0FP46mizchi6kagura4core9inpututil6key__o = 79;
const _M0FP46mizchi6kagura4core9inpututil6key__e = 69;
const _M0FP46mizchi6kagura4core9inpututil10key__space = 32;
const _M0FP46mizchi6kagura4core9inpututil15key__enter__web = 13;
const _M0FP46mizchi6kagura4core9inpututil16key__enter__glfw = 257;
const _M0FP311moonbitlang4core7builtin4seed = _M0FP311moonbitlang4core7builtin12random__seed();
const _M0FP37astrata7astrata3lib22initial__battle__field = { own: _M0FP37astrata7astrata3lib16make__own__field(), rival: _M0FP37astrata7astrata3lib18make__rival__field() };
const _bind = [{ _0: 0, _1: { _0: _M0FP37astrata7astrata4main53____test__67616d655f766965775f7762746573742e6d6274__0, _1: ["Game::new initializes own side selection and initial state"] } }, { _0: 1, _1: { _0: _M0FP37astrata7astrata4main53____test__67616d655f766965775f7762746573742e6d6274__1, _1: ["is_slot_selected matches side zone and slot"] } }, { _0: 2, _1: { _0: _M0FP37astrata7astrata4main53____test__67616d655f766965775f7762746573742e6d6274__2, _1: ["is_slot_selected rejects same slot in a different zone"] } }, { _0: 3, _1: { _0: _M0FP37astrata7astrata4main53____test__67616d655f766965775f7762746573742e6d6274__3, _1: ["is_slot_selected rejects same zone and slot on a different side"] } }, { _0: 4, _1: { _0: _M0FP37astrata7astrata4main53____test__67616d655f766965775f7762746573742e6d6274__4, _1: ["is_slot_selected rejects non-slot selections"] } }];
const _tmp = { _0: "game_view_wbtest.mbt", _1: _M0MP311moonbitlang4core7builtin3Map11from__arrayGiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEE({ buf: _bind, start: 0, end: 5 }) };
const _bind$2 = [{ _0: 0, _1: { _0: _M0FP37astrata7astrata4main57____test__67616d655f7570646174655f7762746573742e6d6274__0, _1: ["update_game handles right key through selection entrypoint"] } }, { _0: 1, _1: { _0: _M0FP37astrata7astrata4main57____test__67616d655f7570646174655f7762746573742e6d6274__1, _1: ["update_game handles left key through selection entrypoint"] } }, { _0: 2, _1: { _0: _M0FP37astrata7astrata4main57____test__67616d655f7570646174655f7762746573742e6d6274__2, _1: ["update_game handles up key through selection entrypoint"] } }, { _0: 3, _1: { _0: _M0FP37astrata7astrata4main57____test__67616d655f7570646174655f7762746573742e6d6274__3, _1: ["update_game handles down key through selection entrypoint"] } }, { _0: 4, _1: { _0: _M0FP37astrata7astrata4main57____test__67616d655f7570646174655f7762746573742e6d6274__4, _1: ["update_game handles space confirm through card play entrypoint"] } }, { _0: 5, _1: { _0: _M0FP37astrata7astrata4main57____test__67616d655f7570646174655f7762746573742e6d6274__5, _1: ["update_game handles enter confirm through card play entrypoint"] } }, { _0: 6, _1: { _0: _M0FP37astrata7astrata4main57____test__67616d655f7570646174655f7762746573742e6d6274__6, _1: ["update_game handles E key through turn entrypoint"] } }];
const _bind$3 = [_tmp, { _0: "game_update_wbtest.mbt", _1: _M0MP311moonbitlang4core7builtin3Map11from__arrayGiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEE({ buf: _bind$2, start: 0, end: 7 }) }];
const _M0FP37astrata7astrata4main48moonbit__test__driver__internal__no__args__tests = _M0MP311moonbitlang4core7builtin3Map11from__arrayGsRP311moonbitlang4core7builtin3MapGiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEEE({ buf: _bind$3, start: 0, end: 2 });
function _M0FP311moonbitlang4core5abort5abortGiE(msg) {
  return $panic();
}
function _M0MP311moonbitlang4core7builtin6Logger13write__objectGRP37astrata7astrata3lib9FieldSideE(self, obj) {
  _M0IP37astrata7astrata3lib9FieldSideP311moonbitlang4core7builtin4Show6output(obj, self);
}
function _M0MP311moonbitlang4core7builtin6Logger13write__objectGRP37astrata7astrata3lib9FieldZoneE(self, obj) {
  _M0IP37astrata7astrata3lib9FieldZoneP311moonbitlang4core7builtin4Show6output(obj, self);
}
function _M0MP311moonbitlang4core7builtin6Logger13write__objectGRP37astrata7astrata3lib15BattleFieldSlotE(self, obj) {
  _M0IP37astrata7astrata3lib15BattleFieldSlotP311moonbitlang4core7builtin4Show6output(obj, self);
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
function _M0FP311moonbitlang4core7builtin4rotl(x, r) {
  return x << r | (x >>> (32 - r | 0) | 0);
}
function _M0MP311moonbitlang4core7builtin6Hasher8consume4(self, input) {
  self.acc = Math.imul(_M0FP311moonbitlang4core7builtin4rotl((self.acc >>> 0) + ((Math.imul(input, -1028477379) | 0) >>> 0) | 0, 17), 668265263) | 0;
}
function _M0MP311moonbitlang4core7builtin6Hasher13combine__uint(self, value) {
  self.acc = (self.acc >>> 0) + (4 >>> 0) | 0;
  _M0MP311moonbitlang4core7builtin6Hasher8consume4(self, value);
}
function _M0FP311moonbitlang4core7builtin5abortGiE(string, loc) {
  return _M0FP311moonbitlang4core5abort5abortGiE(`${_M0IP311moonbitlang4core6string6StringP311moonbitlang4core7builtin4Show10to__string(string)}\n  at ${_M0IP016_24default__implP311moonbitlang4core7builtin4Show10to__stringGRP311moonbitlang4core7builtin9SourceLocE(loc)}\n`);
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
  const _bind$4 = self;
  _bind$4.val = `${_bind$4.val}${String.fromCodePoint(ch)}`;
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
function _M0MP311moonbitlang4core5array5Array2atGRP37astrata7astrata3lib8HandCardE(self, index) {
  const len = self.length;
  if (index >= 0 && index < len) {
    $bound_check(self, index);
    return self[index];
  } else {
    return $panic();
  }
}
function _M0MP311moonbitlang4core7builtin13SourceLocRepr5parse(repr) {
  const _bind$4 = { str: repr, start: 0, end: repr.length };
  const _data = _M0MP311moonbitlang4core6string10StringView4data(_bind$4);
  const _start = _M0MP311moonbitlang4core6string10StringView13start__offset(_bind$4);
  const _end = _start + _M0MP311moonbitlang4core6string10StringView6length(_bind$4) | 0;
  let _cursor = _start;
  let accept_state = -1;
  let match_end = -1;
  let match_tag_saver_0 = -1;
  let match_tag_saver_1 = -1;
  let match_tag_saver_2 = -1;
  let match_tag_saver_3 = -1;
  let match_tag_saver_4 = -1;
  let tag_0 = -1;
  let tag_1 = -1;
  let tag_1_1 = -1;
  let tag_1_2 = -1;
  let tag_3 = -1;
  let tag_2 = -1;
  let tag_2_1 = -1;
  let tag_4 = -1;
  _L: {
    if (_cursor < _end) {
      const next_char = _M0MP311moonbitlang4core6string6String20unsafe__charcode__at(_data, _cursor);
      _cursor = _cursor + 1 | 0;
      if (next_char === 64) {
        _L$2: while (true) {
          tag_0 = _cursor;
          if (_cursor < _end) {
            const next_char$2 = _M0MP311moonbitlang4core6string6String20unsafe__charcode__at(_data, _cursor);
            _cursor = _cursor + 1 | 0;
            if (next_char$2 === 58) {
              if (_cursor < _end) {
                _M0MP311moonbitlang4core6string6String20unsafe__charcode__at(_data, _cursor);
                _cursor = _cursor + 1 | 0;
                let _tmp$2 = 0;
                _L$3: while (true) {
                  const dispatch_15 = _tmp$2;
                  _L$4: {
                    _L$5: {
                      switch (dispatch_15) {
                        case 3: {
                          tag_1_2 = tag_1_1;
                          tag_1_1 = tag_1;
                          tag_1 = _cursor;
                          if (_cursor < _end) {
                            _L$6: {
                              const next_char$3 = _M0MP311moonbitlang4core6string6String20unsafe__charcode__at(_data, _cursor);
                              _cursor = _cursor + 1 | 0;
                              if (next_char$3 < 58) {
                                if (next_char$3 < 48) {
                                  break _L$6;
                                } else {
                                  tag_1 = _cursor;
                                  tag_2_1 = tag_2;
                                  tag_2 = _cursor;
                                  tag_3 = _cursor;
                                  if (_cursor < _end) {
                                    _L$7: {
                                      const next_char$4 = _M0MP311moonbitlang4core6string6String20unsafe__charcode__at(_data, _cursor);
                                      _cursor = _cursor + 1 | 0;
                                      if (next_char$4 < 48) {
                                        if (next_char$4 === 45) {
                                          break _L$4;
                                        } else {
                                          break _L$7;
                                        }
                                      } else {
                                        if (next_char$4 > 57) {
                                          if (next_char$4 < 59) {
                                            _tmp$2 = 3;
                                            continue _L$3;
                                          } else {
                                            break _L$7;
                                          }
                                        } else {
                                          _tmp$2 = 6;
                                          continue _L$3;
                                        }
                                      }
                                    }
                                    _tmp$2 = 0;
                                    continue _L$3;
                                  } else {
                                    break _L;
                                  }
                                }
                              } else {
                                if (next_char$3 > 58) {
                                  break _L$6;
                                } else {
                                  _tmp$2 = 1;
                                  continue _L$3;
                                }
                              }
                            }
                            _tmp$2 = 0;
                            continue _L$3;
                          } else {
                            break _L;
                          }
                        }
                        case 2: {
                          tag_1 = _cursor;
                          tag_2 = _cursor;
                          if (_cursor < _end) {
                            _L$6: {
                              const next_char$3 = _M0MP311moonbitlang4core6string6String20unsafe__charcode__at(_data, _cursor);
                              _cursor = _cursor + 1 | 0;
                              if (next_char$3 < 58) {
                                if (next_char$3 < 48) {
                                  break _L$6;
                                } else {
                                  _tmp$2 = 2;
                                  continue _L$3;
                                }
                              } else {
                                if (next_char$3 > 58) {
                                  break _L$6;
                                } else {
                                  _tmp$2 = 3;
                                  continue _L$3;
                                }
                              }
                            }
                            _tmp$2 = 0;
                            continue _L$3;
                          } else {
                            break _L;
                          }
                        }
                        case 0: {
                          tag_1 = _cursor;
                          if (_cursor < _end) {
                            const next_char$3 = _M0MP311moonbitlang4core6string6String20unsafe__charcode__at(_data, _cursor);
                            _cursor = _cursor + 1 | 0;
                            if (next_char$3 === 58) {
                              _tmp$2 = 1;
                              continue _L$3;
                            } else {
                              _tmp$2 = 0;
                              continue _L$3;
                            }
                          } else {
                            break _L;
                          }
                        }
                        case 4: {
                          tag_1 = _cursor;
                          tag_4 = _cursor;
                          if (_cursor < _end) {
                            _L$6: {
                              const next_char$3 = _M0MP311moonbitlang4core6string6String20unsafe__charcode__at(_data, _cursor);
                              _cursor = _cursor + 1 | 0;
                              if (next_char$3 < 58) {
                                if (next_char$3 < 48) {
                                  break _L$6;
                                } else {
                                  _tmp$2 = 4;
                                  continue _L$3;
                                }
                              } else {
                                if (next_char$3 > 58) {
                                  break _L$6;
                                } else {
                                  tag_1_2 = tag_1_1;
                                  tag_1_1 = tag_1;
                                  tag_1 = _cursor;
                                  if (_cursor < _end) {
                                    _L$7: {
                                      const next_char$4 = _M0MP311moonbitlang4core6string6String20unsafe__charcode__at(_data, _cursor);
                                      _cursor = _cursor + 1 | 0;
                                      if (next_char$4 < 58) {
                                        if (next_char$4 < 48) {
                                          break _L$7;
                                        } else {
                                          tag_1 = _cursor;
                                          tag_2_1 = tag_2;
                                          tag_2 = _cursor;
                                          if (_cursor < _end) {
                                            _L$8: {
                                              const next_char$5 = _M0MP311moonbitlang4core6string6String20unsafe__charcode__at(_data, _cursor);
                                              _cursor = _cursor + 1 | 0;
                                              if (next_char$5 < 58) {
                                                if (next_char$5 < 48) {
                                                  break _L$8;
                                                } else {
                                                  _tmp$2 = 5;
                                                  continue _L$3;
                                                }
                                              } else {
                                                if (next_char$5 > 58) {
                                                  break _L$8;
                                                } else {
                                                  _tmp$2 = 3;
                                                  continue _L$3;
                                                }
                                              }
                                            }
                                            _tmp$2 = 0;
                                            continue _L$3;
                                          } else {
                                            break _L$5;
                                          }
                                        }
                                      } else {
                                        if (next_char$4 > 58) {
                                          break _L$7;
                                        } else {
                                          _tmp$2 = 1;
                                          continue _L$3;
                                        }
                                      }
                                    }
                                    _tmp$2 = 0;
                                    continue _L$3;
                                  } else {
                                    break _L;
                                  }
                                }
                              }
                            }
                            _tmp$2 = 0;
                            continue _L$3;
                          } else {
                            break _L;
                          }
                        }
                        case 5: {
                          tag_1 = _cursor;
                          tag_2 = _cursor;
                          if (_cursor < _end) {
                            _L$6: {
                              const next_char$3 = _M0MP311moonbitlang4core6string6String20unsafe__charcode__at(_data, _cursor);
                              _cursor = _cursor + 1 | 0;
                              if (next_char$3 < 58) {
                                if (next_char$3 < 48) {
                                  break _L$6;
                                } else {
                                  _tmp$2 = 5;
                                  continue _L$3;
                                }
                              } else {
                                if (next_char$3 > 58) {
                                  break _L$6;
                                } else {
                                  _tmp$2 = 3;
                                  continue _L$3;
                                }
                              }
                            }
                            _tmp$2 = 0;
                            continue _L$3;
                          } else {
                            break _L$5;
                          }
                        }
                        case 6: {
                          tag_1 = _cursor;
                          tag_2 = _cursor;
                          tag_3 = _cursor;
                          if (_cursor < _end) {
                            _L$6: {
                              const next_char$3 = _M0MP311moonbitlang4core6string6String20unsafe__charcode__at(_data, _cursor);
                              _cursor = _cursor + 1 | 0;
                              if (next_char$3 < 48) {
                                if (next_char$3 === 45) {
                                  break _L$4;
                                } else {
                                  break _L$6;
                                }
                              } else {
                                if (next_char$3 > 57) {
                                  if (next_char$3 < 59) {
                                    _tmp$2 = 3;
                                    continue _L$3;
                                  } else {
                                    break _L$6;
                                  }
                                } else {
                                  _tmp$2 = 6;
                                  continue _L$3;
                                }
                              }
                            }
                            _tmp$2 = 0;
                            continue _L$3;
                          } else {
                            break _L;
                          }
                        }
                        case 1: {
                          tag_1_1 = tag_1;
                          tag_1 = _cursor;
                          if (_cursor < _end) {
                            _L$6: {
                              const next_char$3 = _M0MP311moonbitlang4core6string6String20unsafe__charcode__at(_data, _cursor);
                              _cursor = _cursor + 1 | 0;
                              if (next_char$3 < 58) {
                                if (next_char$3 < 48) {
                                  break _L$6;
                                } else {
                                  _tmp$2 = 2;
                                  continue _L$3;
                                }
                              } else {
                                if (next_char$3 > 58) {
                                  break _L$6;
                                } else {
                                  _tmp$2 = 1;
                                  continue _L$3;
                                }
                              }
                            }
                            _tmp$2 = 0;
                            continue _L$3;
                          } else {
                            break _L;
                          }
                        }
                        default: {
                          break _L;
                        }
                      }
                    }
                    tag_1 = tag_1_2;
                    tag_2 = tag_2_1;
                    match_tag_saver_0 = tag_0;
                    match_tag_saver_1 = tag_1;
                    match_tag_saver_2 = tag_2;
                    match_tag_saver_3 = tag_3;
                    match_tag_saver_4 = tag_4;
                    accept_state = 0;
                    match_end = _cursor;
                    break _L;
                  }
                  tag_1_1 = tag_1_2;
                  tag_1 = _cursor;
                  tag_2 = tag_2_1;
                  if (_cursor < _end) {
                    _L$5: {
                      const next_char$3 = _M0MP311moonbitlang4core6string6String20unsafe__charcode__at(_data, _cursor);
                      _cursor = _cursor + 1 | 0;
                      if (next_char$3 < 58) {
                        if (next_char$3 < 48) {
                          break _L$5;
                        } else {
                          _tmp$2 = 4;
                          continue;
                        }
                      } else {
                        if (next_char$3 > 58) {
                          break _L$5;
                        } else {
                          _tmp$2 = 1;
                          continue;
                        }
                      }
                    }
                    _tmp$2 = 0;
                    continue;
                  } else {
                    break _L;
                  }
                }
              } else {
                break _L;
              }
            } else {
              continue;
            }
          } else {
            break _L;
          }
        }
      } else {
        break _L;
      }
    } else {
      break _L;
    }
  }
  if (accept_state === 0) {
    let start_line;
    let _try_err;
    _L$2: {
      _L$3: {
        const _bind$5 = _M0MP311moonbitlang4core6string6String3sub(_data, match_tag_saver_1 + 1 | 0, match_tag_saver_2);
        if (_bind$5.$tag === 1) {
          const _ok = _bind$5;
          start_line = _ok._0;
        } else {
          const _err = _bind$5;
          _try_err = _err._0;
          break _L$3;
        }
        break _L$2;
      }
      start_line = $panic();
    }
    let start_column;
    let _try_err$2;
    _L$3: {
      _L$4: {
        const _bind$5 = _M0MP311moonbitlang4core6string6String3sub(_data, match_tag_saver_2 + 1 | 0, match_tag_saver_3);
        if (_bind$5.$tag === 1) {
          const _ok = _bind$5;
          start_column = _ok._0;
        } else {
          const _err = _bind$5;
          _try_err$2 = _err._0;
          break _L$4;
        }
        break _L$3;
      }
      start_column = $panic();
    }
    let pkg;
    let _try_err$3;
    _L$4: {
      _L$5: {
        const _bind$5 = _M0MP311moonbitlang4core6string6String3sub(_data, _start + 1 | 0, match_tag_saver_0);
        if (_bind$5.$tag === 1) {
          const _ok = _bind$5;
          pkg = _ok._0;
        } else {
          const _err = _bind$5;
          _try_err$3 = _err._0;
          break _L$5;
        }
        break _L$4;
      }
      pkg = $panic();
    }
    let filename;
    let _try_err$4;
    _L$5: {
      _L$6: {
        const _bind$5 = _M0MP311moonbitlang4core6string6String3sub(_data, match_tag_saver_0 + 1 | 0, match_tag_saver_1);
        if (_bind$5.$tag === 1) {
          const _ok = _bind$5;
          filename = _ok._0;
        } else {
          const _err = _bind$5;
          _try_err$4 = _err._0;
          break _L$6;
        }
        break _L$5;
      }
      filename = $panic();
    }
    let end_line;
    let _try_err$5;
    _L$6: {
      _L$7: {
        const _bind$5 = _M0MP311moonbitlang4core6string6String3sub(_data, match_tag_saver_3 + 1 | 0, match_tag_saver_4);
        if (_bind$5.$tag === 1) {
          const _ok = _bind$5;
          end_line = _ok._0;
        } else {
          const _err = _bind$5;
          _try_err$5 = _err._0;
          break _L$7;
        }
        break _L$6;
      }
      end_line = $panic();
    }
    let end_column;
    let _try_err$6;
    _L$7: {
      _L$8: {
        const _bind$5 = _M0MP311moonbitlang4core6string6String3sub(_data, match_tag_saver_4 + 1 | 0, match_end);
        if (_bind$5.$tag === 1) {
          const _ok = _bind$5;
          end_column = _ok._0;
        } else {
          const _err = _bind$5;
          _try_err$6 = _err._0;
          break _L$8;
        }
        break _L$7;
      }
      end_column = $panic();
    }
    return { pkg: pkg, filename: filename, start_line: start_line, start_column: start_column, end_line: end_line, end_column: end_column };
  } else {
    return $panic();
  }
}
function _M0IP311moonbitlang4core7builtin13StringBuilderP311moonbitlang4core7builtin6Logger13write__string(self, str) {
  const _bind$4 = self;
  _bind$4.val = `${_bind$4.val}${str}`;
}
function _M0IP311moonbitlang4core6uint166UInt16P311moonbitlang4core7builtin7Compare7compare(self, that) {
  return $compare_int(self, that);
}
function _M0MP311moonbitlang4core7builtin6Hasher12combine__int(self, value) {
  _M0MP311moonbitlang4core7builtin6Hasher13combine__uint(self, value);
}
function _M0MP311moonbitlang4core7builtin6Hasher7combineGiE(self, value) {
  _M0IP311moonbitlang4core3int3IntP311moonbitlang4core7builtin4Hash13hash__combine(value, self);
}
function _M0MP311moonbitlang4core7builtin6Hasher7combineGsE(self, value) {
  _M0IP311moonbitlang4core6string6StringP311moonbitlang4core7builtin4Hash13hash__combine(value, self);
}
function _M0IP016_24default__implP311moonbitlang4core7builtin2Eq10not__equalGRP37astrata7astrata3lib9SelectionE(x, y) {
  return !_M0IP37astrata7astrata3lib9SelectionP311moonbitlang4core7builtin2Eq5equal(x, y);
}
function _M0IP016_24default__implP311moonbitlang4core7builtin2Eq10not__equalGRP37astrata7astrata3lib9FieldSideE(x, y) {
  return !_M0IP37astrata7astrata3lib9FieldSideP311moonbitlang4core7builtin2Eq5equal(x, y);
}
function _M0IP016_24default__implP311moonbitlang4core7builtin2Eq10not__equalGsE(x, y) {
  return !(x === y);
}
function _M0IP016_24default__implP311moonbitlang4core7builtin2Eq10not__equalGbE(x, y) {
  return !(x === y);
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
function _M0MP311moonbitlang4core7builtin6Hasher9avalanche(self) {
  let acc = self.acc;
  acc = acc ^ (acc >>> 15 | 0);
  acc = Math.imul(acc, -2048144777) | 0;
  acc = acc ^ (acc >>> 13 | 0);
  acc = Math.imul(acc, -1028477379) | 0;
  acc = acc ^ (acc >>> 16 | 0);
  return acc;
}
function _M0MP311moonbitlang4core7builtin6Hasher8finalize(self) {
  return _M0MP311moonbitlang4core7builtin6Hasher9avalanche(self);
}
function _M0MP311moonbitlang4core7builtin6Hasher11new_2einner(seed) {
  return { acc: (seed >>> 0) + (374761393 >>> 0) | 0 };
}
function _M0MP311moonbitlang4core7builtin6Hasher3new(seed$46$opt) {
  let seed;
  if (seed$46$opt === undefined) {
    seed = _M0FP311moonbitlang4core7builtin4seed;
  } else {
    const _Some = seed$46$opt;
    seed = _Some;
  }
  return _M0MP311moonbitlang4core7builtin6Hasher11new_2einner(seed);
}
function _M0IP016_24default__implP311moonbitlang4core7builtin4Hash4hashGiE(self) {
  const h = _M0MP311moonbitlang4core7builtin6Hasher3new(undefined);
  _M0MP311moonbitlang4core7builtin6Hasher7combineGiE(h, self);
  return _M0MP311moonbitlang4core7builtin6Hasher8finalize(h);
}
function _M0IP016_24default__implP311moonbitlang4core7builtin4Hash4hashGsE(self) {
  const h = _M0MP311moonbitlang4core7builtin6Hasher3new(undefined);
  _M0MP311moonbitlang4core7builtin6Hasher7combineGsE(h, self);
  return _M0MP311moonbitlang4core7builtin6Hasher8finalize(h);
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
function _M0MP311moonbitlang4core6string6String3sub(self, start$46$opt, end) {
  let start;
  if (start$46$opt === undefined) {
    start = 0;
  } else {
    const _Some = start$46$opt;
    start = _Some;
  }
  return _M0MP311moonbitlang4core6string6String11sub_2einner(self, start, end);
}
function _M0IP016_24default__implP311moonbitlang4core7builtin6Logger16write__substringGRP311moonbitlang4core7builtin13StringBuilderE(self, value, start, len) {
  let _tmp$2;
  let _try_err;
  _L: {
    _L$2: {
      const _bind$4 = _M0MP311moonbitlang4core6string6String11sub_2einner(value, start, start + len | 0);
      if (_bind$4.$tag === 1) {
        const _ok = _bind$4;
        _tmp$2 = _ok._0;
      } else {
        const _err = _bind$4;
        _try_err = _err._0;
        break _L$2;
      }
      break _L;
    }
    _tmp$2 = $panic();
  }
  _M0IP311moonbitlang4core7builtin13StringBuilderP311moonbitlang4core7builtin6Logger11write__view(self, _tmp$2);
}
function _M0MP311moonbitlang4core6string10StringView4data(self) {
  return self.str;
}
function _M0MP311moonbitlang4core6string10StringView6length(self) {
  return self.end - self.start | 0;
}
function _M0MP311moonbitlang4core6string10StringView13start__offset(self) {
  return self.start;
}
function _M0IP016_24default__implP311moonbitlang4core7builtin4Show10to__stringGiE(self) {
  const logger = _M0MP311moonbitlang4core7builtin13StringBuilder11new_2einner(0);
  _M0IP311moonbitlang4core3int3IntP311moonbitlang4core7builtin4Show6output(self, { self: logger, method_table: _M0FP095_40moonbitlang_2fcore_2fbuiltin_2eStringBuilder_24as_24_40moonbitlang_2fcore_2fbuiltin_2eLogger });
  return _M0MP311moonbitlang4core7builtin13StringBuilder10to__string(logger);
}
function _M0IP016_24default__implP311moonbitlang4core7builtin4Show10to__stringGRP37astrata7astrata3lib5PhaseE(self) {
  const logger = _M0MP311moonbitlang4core7builtin13StringBuilder11new_2einner(0);
  _M0IP37astrata7astrata3lib5PhaseP311moonbitlang4core7builtin4Show6output(self, { self: logger, method_table: _M0FP095_40moonbitlang_2fcore_2fbuiltin_2eStringBuilder_24as_24_40moonbitlang_2fcore_2fbuiltin_2eLogger });
  return _M0MP311moonbitlang4core7builtin13StringBuilder10to__string(logger);
}
function _M0IP016_24default__implP311moonbitlang4core7builtin4Show10to__stringGRP311moonbitlang4core7builtin9SourceLocE(self) {
  const logger = _M0MP311moonbitlang4core7builtin13StringBuilder11new_2einner(0);
  _M0IP311moonbitlang4core7builtin9SourceLocP311moonbitlang4core7builtin4Show6output(self, { self: logger, method_table: _M0FP095_40moonbitlang_2fcore_2fbuiltin_2eStringBuilder_24as_24_40moonbitlang_2fcore_2fbuiltin_2eLogger });
  return _M0MP311moonbitlang4core7builtin13StringBuilder10to__string(logger);
}
function _M0IP016_24default__implP311moonbitlang4core7builtin4Show10to__stringGRP37astrata7astrata3lib9FieldSideE(self) {
  const logger = _M0MP311moonbitlang4core7builtin13StringBuilder11new_2einner(0);
  _M0IP37astrata7astrata3lib9FieldSideP311moonbitlang4core7builtin4Show6output(self, { self: logger, method_table: _M0FP095_40moonbitlang_2fcore_2fbuiltin_2eStringBuilder_24as_24_40moonbitlang_2fcore_2fbuiltin_2eLogger });
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
function _M0FP311moonbitlang4core7builtin13debug__stringGRP37astrata7astrata3lib9SelectionE(t) {
  const buf = _M0MP311moonbitlang4core7builtin13StringBuilder11new_2einner(50);
  _M0IP37astrata7astrata3lib9SelectionP311moonbitlang4core7builtin4Show6output(t, { self: buf, method_table: _M0FP095_40moonbitlang_2fcore_2fbuiltin_2eStringBuilder_24as_24_40moonbitlang_2fcore_2fbuiltin_2eLogger });
  return _M0MP311moonbitlang4core7builtin13StringBuilder10to__string(buf);
}
function _M0FP311moonbitlang4core7builtin13debug__stringGRP37astrata7astrata3lib9FieldSideE(t) {
  const buf = _M0MP311moonbitlang4core7builtin13StringBuilder11new_2einner(50);
  _M0IP37astrata7astrata3lib9FieldSideP311moonbitlang4core7builtin4Show6output(t, { self: buf, method_table: _M0FP095_40moonbitlang_2fcore_2fbuiltin_2eStringBuilder_24as_24_40moonbitlang_2fcore_2fbuiltin_2eLogger });
  return _M0MP311moonbitlang4core7builtin13StringBuilder10to__string(buf);
}
function _M0FP311moonbitlang4core7builtin13debug__stringGsE(t) {
  const buf = _M0MP311moonbitlang4core7builtin13StringBuilder11new_2einner(50);
  _M0IP311moonbitlang4core6string6StringP311moonbitlang4core7builtin4Show6output(t, { self: buf, method_table: _M0FP095_40moonbitlang_2fcore_2fbuiltin_2eStringBuilder_24as_24_40moonbitlang_2fcore_2fbuiltin_2eLogger });
  return _M0MP311moonbitlang4core7builtin13StringBuilder10to__string(buf);
}
function _M0FP311moonbitlang4core7builtin13debug__stringGiE(t) {
  const buf = _M0MP311moonbitlang4core7builtin13StringBuilder11new_2einner(50);
  _M0IP311moonbitlang4core3int3IntP311moonbitlang4core7builtin4Show6output(t, { self: buf, method_table: _M0FP095_40moonbitlang_2fcore_2fbuiltin_2eStringBuilder_24as_24_40moonbitlang_2fcore_2fbuiltin_2eLogger });
  return _M0MP311moonbitlang4core7builtin13StringBuilder10to__string(buf);
}
function _M0FP311moonbitlang4core7builtin13debug__stringGbE(t) {
  const buf = _M0MP311moonbitlang4core7builtin13StringBuilder11new_2einner(50);
  _M0IP311moonbitlang4core4bool4BoolP311moonbitlang4core7builtin4Show6output(t, { self: buf, method_table: _M0FP095_40moonbitlang_2fcore_2fbuiltin_2eStringBuilder_24as_24_40moonbitlang_2fcore_2fbuiltin_2eLogger });
  return _M0MP311moonbitlang4core7builtin13StringBuilder10to__string(buf);
}
function _M0FP311moonbitlang4core7builtin4failGuE(msg, loc) {
  return new Result$Err$1$(new Error$moonbitlang$47$core$47$builtin$46$Failure$46$Failure(`${_M0IP016_24default__implP311moonbitlang4core7builtin4Show10to__stringGRP311moonbitlang4core7builtin9SourceLocE(loc)} FAILED: ${_M0IP311moonbitlang4core6string6StringP311moonbitlang4core7builtin4Show10to__string(msg)}`));
}
function _M0FP311moonbitlang4core7builtin10assert__eqGRP37astrata7astrata3lib9SelectionE(a, b, msg, loc) {
  if (_M0IP016_24default__implP311moonbitlang4core7builtin2Eq10not__equalGRP37astrata7astrata3lib9SelectionE(a, b)) {
    let fail_msg;
    if (msg === undefined) {
      fail_msg = `\`${_M0IP311moonbitlang4core6string6StringP311moonbitlang4core7builtin4Show10to__string(_M0FP311moonbitlang4core7builtin13debug__stringGRP37astrata7astrata3lib9SelectionE(a))} != ${_M0IP311moonbitlang4core6string6StringP311moonbitlang4core7builtin4Show10to__string(_M0FP311moonbitlang4core7builtin13debug__stringGRP37astrata7astrata3lib9SelectionE(b))}\``;
    } else {
      const _Some = msg;
      fail_msg = _Some;
    }
    return _M0FP311moonbitlang4core7builtin4failGuE(fail_msg, loc);
  } else {
    return new Result$Ok$2$(undefined);
  }
}
function _M0FP311moonbitlang4core7builtin10assert__eqGRP37astrata7astrata3lib9FieldSideE(a, b, msg, loc) {
  if (_M0IP016_24default__implP311moonbitlang4core7builtin2Eq10not__equalGRP37astrata7astrata3lib9FieldSideE(a, b)) {
    let fail_msg;
    if (msg === undefined) {
      fail_msg = `\`${_M0IP311moonbitlang4core6string6StringP311moonbitlang4core7builtin4Show10to__string(_M0FP311moonbitlang4core7builtin13debug__stringGRP37astrata7astrata3lib9FieldSideE(a))} != ${_M0IP311moonbitlang4core6string6StringP311moonbitlang4core7builtin4Show10to__string(_M0FP311moonbitlang4core7builtin13debug__stringGRP37astrata7astrata3lib9FieldSideE(b))}\``;
    } else {
      const _Some = msg;
      fail_msg = _Some;
    }
    return _M0FP311moonbitlang4core7builtin4failGuE(fail_msg, loc);
  } else {
    return new Result$Ok$2$(undefined);
  }
}
function _M0FP311moonbitlang4core7builtin10assert__eqGsE(a, b, msg, loc) {
  if (_M0IP016_24default__implP311moonbitlang4core7builtin2Eq10not__equalGsE(a, b)) {
    let fail_msg;
    if (msg === undefined) {
      fail_msg = `\`${_M0IP311moonbitlang4core6string6StringP311moonbitlang4core7builtin4Show10to__string(_M0FP311moonbitlang4core7builtin13debug__stringGsE(a))} != ${_M0IP311moonbitlang4core6string6StringP311moonbitlang4core7builtin4Show10to__string(_M0FP311moonbitlang4core7builtin13debug__stringGsE(b))}\``;
    } else {
      const _Some = msg;
      fail_msg = _Some;
    }
    return _M0FP311moonbitlang4core7builtin4failGuE(fail_msg, loc);
  } else {
    return new Result$Ok$2$(undefined);
  }
}
function _M0FP311moonbitlang4core7builtin10assert__eqGiE(a, b, msg, loc) {
  if (a !== b) {
    let fail_msg;
    if (msg === undefined) {
      fail_msg = `\`${_M0IP311moonbitlang4core6string6StringP311moonbitlang4core7builtin4Show10to__string(_M0FP311moonbitlang4core7builtin13debug__stringGiE(a))} != ${_M0IP311moonbitlang4core6string6StringP311moonbitlang4core7builtin4Show10to__string(_M0FP311moonbitlang4core7builtin13debug__stringGiE(b))}\``;
    } else {
      const _Some = msg;
      fail_msg = _Some;
    }
    return _M0FP311moonbitlang4core7builtin4failGuE(fail_msg, loc);
  } else {
    return new Result$Ok$2$(undefined);
  }
}
function _M0FP311moonbitlang4core7builtin10assert__eqGbE(a, b, msg, loc) {
  if (_M0IP016_24default__implP311moonbitlang4core7builtin2Eq10not__equalGbE(a, b)) {
    let fail_msg;
    if (msg === undefined) {
      fail_msg = `\`${_M0IP311moonbitlang4core6string6StringP311moonbitlang4core7builtin4Show10to__string(_M0FP311moonbitlang4core7builtin13debug__stringGbE(a))} != ${_M0IP311moonbitlang4core6string6StringP311moonbitlang4core7builtin4Show10to__string(_M0FP311moonbitlang4core7builtin13debug__stringGbE(b))}\``;
    } else {
      const _Some = msg;
      fail_msg = _Some;
    }
    return _M0FP311moonbitlang4core7builtin4failGuE(fail_msg, loc);
  } else {
    return new Result$Ok$2$(undefined);
  }
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
  let _tmp$2 = 0;
  let _tmp$3 = 0;
  _L: while (true) {
    const i = _tmp$2;
    const seg = _tmp$3;
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
          _tmp$2 = i + 1 | 0;
          _tmp$3 = i + 1 | 0;
          continue _L;
        }
        case 13: {
          _M0FP311moonbitlang4core7builtin30output_2eflush__segment_7c3845(_env, seg, i);
          logger.method_table.method_0(logger.self, "\\r");
          _tmp$2 = i + 1 | 0;
          _tmp$3 = i + 1 | 0;
          continue _L;
        }
        case 8: {
          _M0FP311moonbitlang4core7builtin30output_2eflush__segment_7c3845(_env, seg, i);
          logger.method_table.method_0(logger.self, "\\b");
          _tmp$2 = i + 1 | 0;
          _tmp$3 = i + 1 | 0;
          continue _L;
        }
        case 9: {
          _M0FP311moonbitlang4core7builtin30output_2eflush__segment_7c3845(_env, seg, i);
          logger.method_table.method_0(logger.self, "\\t");
          _tmp$2 = i + 1 | 0;
          _tmp$3 = i + 1 | 0;
          continue _L;
        }
        default: {
          if (_M0IP016_24default__implP311moonbitlang4core7builtin7Compare6op__ltGkE(code, 32)) {
            _M0FP311moonbitlang4core7builtin30output_2eflush__segment_7c3845(_env, seg, i);
            logger.method_table.method_0(logger.self, "\\u{");
            logger.method_table.method_0(logger.self, _M0MP311moonbitlang4core4byte4Byte7to__hex(code & 255));
            logger.method_table.method_3(logger.self, 125);
            _tmp$2 = i + 1 | 0;
            _tmp$3 = i + 1 | 0;
            continue _L;
          } else {
            _tmp$2 = i + 1 | 0;
            continue _L;
          }
        }
      }
    }
    _M0FP311moonbitlang4core7builtin30output_2eflush__segment_7c3845(_env, seg, i);
    logger.method_table.method_3(logger.self, 92);
    logger.method_table.method_3(logger.self, _M0MP311moonbitlang4core6uint166UInt1616unsafe__to__char(c));
    _tmp$2 = i + 1 | 0;
    _tmp$3 = i + 1 | 0;
    continue;
  }
  logger.method_table.method_3(logger.self, 34);
}
function _M0IP311moonbitlang4core6string10StringViewP311moonbitlang4core7builtin4Show10to__string(self) {
  return self.str.substring(self.start, self.end);
}
function _M0MP311moonbitlang4core5array9ArrayView6lengthGUsRP311moonbitlang4core7builtin3MapGiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEEEE(self) {
  return self.end - self.start | 0;
}
function _M0MP311moonbitlang4core5array9ArrayView6lengthGUiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEEE(self) {
  return self.end - self.start | 0;
}
function _M0IP311moonbitlang4core7builtin13StringBuilderP311moonbitlang4core7builtin6Logger11write__view(self, str) {
  const _bind$4 = self;
  _bind$4.val = `${_bind$4.val}${_M0IP311moonbitlang4core6string10StringViewP311moonbitlang4core7builtin4Show10to__string(str)}`;
}
function _M0MP311moonbitlang4core5array5Array4pushGsE(self, value) {
  _M0MP311moonbitlang4core7builtin7JSArray4push(self, value);
}
function _M0MP311moonbitlang4core5array5Array4pushGiE(self, value) {
  _M0MP311moonbitlang4core7builtin7JSArray4push(self, value);
}
function _M0MP311moonbitlang4core5array5Array4pushGRP46mizchi6kagura4core9inpututil16KeyDurationEntryE(self, value) {
  _M0MP311moonbitlang4core7builtin7JSArray4push(self, value);
}
function _M0MP311moonbitlang4core5array5Array4pushGRP46mizchi6kagura4core9inpututil24MouseButtonDurationEntryE(self, value) {
  _M0MP311moonbitlang4core7builtin7JSArray4push(self, value);
}
function _M0MP311moonbitlang4core5array5Array4pushGRP46mizchi6kagura4core9inpututil18TouchDurationEntryE(self, value) {
  _M0MP311moonbitlang4core7builtin7JSArray4push(self, value);
}
function _M0MP311moonbitlang4core5array5Array4pushGRP37astrata7astrata3lib8HandCardE(self, value) {
  _M0MP311moonbitlang4core7builtin7JSArray4push(self, value);
}
function _M0IP311moonbitlang4core4bool4BoolP311moonbitlang4core7builtin4Show6output(self, logger) {
  if (self) {
    logger.method_table.method_0(logger.self, "true");
    return;
  } else {
    logger.method_table.method_0(logger.self, "false");
    return;
  }
}
function _M0IP311moonbitlang4core3int3IntP311moonbitlang4core7builtin4Show6output(self, logger) {
  logger.method_table.method_0(logger.self, _M0MP311moonbitlang4core3int3Int18to__string_2einner(self, 10));
}
function _M0IP311moonbitlang4core6string6StringP311moonbitlang4core7builtin4Show10to__string(self) {
  return self;
}
function _M0MP311moonbitlang4core6option6Option6unwrapGRP311moonbitlang4core7builtin5EntryGsRP311moonbitlang4core7builtin3MapGiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEEEE(self) {
  if (self === undefined) {
    return $panic();
  } else {
    const _Some = self;
    return _Some;
  }
}
function _M0MP311moonbitlang4core6option6Option6unwrapGRP311moonbitlang4core7builtin5EntryGiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEEE(self) {
  if (self === undefined) {
    return $panic();
  } else {
    const _Some = self;
    return _Some;
  }
}
function _M0FP311moonbitlang4core7builtin21calc__grow__threshold(capacity) {
  if (16 === 0) {
    $panic();
  }
  return (Math.imul(capacity, 13) | 0) / 16 | 0;
}
function _M0MP311moonbitlang4core3int3Int20next__power__of__two(self) {
  if (self >= 0) {
    if (self <= 1) {
      return 1;
    }
    if (self > 1073741824) {
      return 1073741824;
    }
    return (2147483647 >> (Math.clz32(self - 1 | 0) - 1 | 0)) + 1 | 0;
  } else {
    return $panic();
  }
}
function _M0MP311moonbitlang4core7builtin3Map11new_2einnerGsRP311moonbitlang4core7builtin3MapGiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEEE(capacity) {
  const capacity$2 = _M0MP311moonbitlang4core3int3Int20next__power__of__two(capacity);
  const _bind$4 = capacity$2 - 1 | 0;
  const _bind$5 = _M0FP311moonbitlang4core7builtin21calc__grow__threshold(capacity$2);
  const _bind$6 = $make_array_len_and_init(capacity$2, undefined);
  const _bind$7 = undefined;
  return { entries: _bind$6, size: 0, capacity: capacity$2, capacity_mask: _bind$4, grow_at: _bind$5, head: _bind$7, tail: -1 };
}
function _M0MP311moonbitlang4core7builtin3Map11new_2einnerGiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEE(capacity) {
  const capacity$2 = _M0MP311moonbitlang4core3int3Int20next__power__of__two(capacity);
  const _bind$4 = capacity$2 - 1 | 0;
  const _bind$5 = _M0FP311moonbitlang4core7builtin21calc__grow__threshold(capacity$2);
  const _bind$6 = $make_array_len_and_init(capacity$2, undefined);
  const _bind$7 = undefined;
  return { entries: _bind$6, size: 0, capacity: capacity$2, capacity_mask: _bind$4, grow_at: _bind$5, head: _bind$7, tail: -1 };
}
function _M0MP311moonbitlang4core7builtin3Map20add__entry__to__tailGsRP311moonbitlang4core7builtin3MapGiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEEE(self, idx, entry) {
  const _bind$4 = self.tail;
  if (_bind$4 === -1) {
    self.head = entry;
  } else {
    const _tmp$2 = self.entries;
    $bound_check(_tmp$2, _bind$4);
    _M0MP311moonbitlang4core6option6Option6unwrapGRP311moonbitlang4core7builtin5EntryGsRP311moonbitlang4core7builtin3MapGiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEEEE(_tmp$2[_bind$4]).next = entry;
  }
  self.tail = idx;
  const _tmp$2 = self.entries;
  $bound_check(_tmp$2, idx);
  _tmp$2[idx] = entry;
  self.size = self.size + 1 | 0;
}
function _M0MP311moonbitlang4core7builtin3Map20add__entry__to__tailGiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEE(self, idx, entry) {
  const _bind$4 = self.tail;
  if (_bind$4 === -1) {
    self.head = entry;
  } else {
    const _tmp$2 = self.entries;
    $bound_check(_tmp$2, _bind$4);
    _M0MP311moonbitlang4core6option6Option6unwrapGRP311moonbitlang4core7builtin5EntryGiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEEE(_tmp$2[_bind$4]).next = entry;
  }
  self.tail = idx;
  const _tmp$2 = self.entries;
  $bound_check(_tmp$2, idx);
  _tmp$2[idx] = entry;
  self.size = self.size + 1 | 0;
}
function _M0MP311moonbitlang4core7builtin3Map10set__entryGsRP311moonbitlang4core7builtin3MapGiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEEE(self, entry, new_idx) {
  const _tmp$2 = self.entries;
  $bound_check(_tmp$2, new_idx);
  _tmp$2[new_idx] = entry;
  const _bind$4 = entry.next;
  if (_bind$4 === undefined) {
    self.tail = new_idx;
    return;
  } else {
    const _Some = _bind$4;
    const _next = _Some;
    _next.prev = new_idx;
    return;
  }
}
function _M0MP311moonbitlang4core7builtin3Map10set__entryGiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEE(self, entry, new_idx) {
  const _tmp$2 = self.entries;
  $bound_check(_tmp$2, new_idx);
  _tmp$2[new_idx] = entry;
  const _bind$4 = entry.next;
  if (_bind$4 === undefined) {
    self.tail = new_idx;
    return;
  } else {
    const _Some = _bind$4;
    const _next = _Some;
    _next.prev = new_idx;
    return;
  }
}
function _M0MP311moonbitlang4core7builtin3Map10push__awayGsRP311moonbitlang4core7builtin3MapGiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEEE(self, idx, entry) {
  let _tmp$2 = entry.psl + 1 | 0;
  let _tmp$3 = (idx + 1 | 0) & self.capacity_mask;
  let _tmp$4 = entry;
  while (true) {
    const psl = _tmp$2;
    const idx$2 = _tmp$3;
    const entry$2 = _tmp$4;
    const _tmp$5 = self.entries;
    $bound_check(_tmp$5, idx$2);
    const _bind$4 = _tmp$5[idx$2];
    if (_bind$4 === undefined) {
      entry$2.psl = psl;
      _M0MP311moonbitlang4core7builtin3Map10set__entryGsRP311moonbitlang4core7builtin3MapGiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEEE(self, entry$2, idx$2);
      break;
    } else {
      const _Some = _bind$4;
      const _curr_entry = _Some;
      if (psl > _curr_entry.psl) {
        entry$2.psl = psl;
        _M0MP311moonbitlang4core7builtin3Map10set__entryGsRP311moonbitlang4core7builtin3MapGiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEEE(self, entry$2, idx$2);
        _tmp$2 = _curr_entry.psl + 1 | 0;
        _tmp$3 = (idx$2 + 1 | 0) & self.capacity_mask;
        _tmp$4 = _curr_entry;
        continue;
      } else {
        _tmp$2 = psl + 1 | 0;
        _tmp$3 = (idx$2 + 1 | 0) & self.capacity_mask;
        continue;
      }
    }
  }
}
function _M0MP311moonbitlang4core7builtin3Map10push__awayGiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEE(self, idx, entry) {
  let _tmp$2 = entry.psl + 1 | 0;
  let _tmp$3 = (idx + 1 | 0) & self.capacity_mask;
  let _tmp$4 = entry;
  while (true) {
    const psl = _tmp$2;
    const idx$2 = _tmp$3;
    const entry$2 = _tmp$4;
    const _tmp$5 = self.entries;
    $bound_check(_tmp$5, idx$2);
    const _bind$4 = _tmp$5[idx$2];
    if (_bind$4 === undefined) {
      entry$2.psl = psl;
      _M0MP311moonbitlang4core7builtin3Map10set__entryGiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEE(self, entry$2, idx$2);
      break;
    } else {
      const _Some = _bind$4;
      const _curr_entry = _Some;
      if (psl > _curr_entry.psl) {
        entry$2.psl = psl;
        _M0MP311moonbitlang4core7builtin3Map10set__entryGiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEE(self, entry$2, idx$2);
        _tmp$2 = _curr_entry.psl + 1 | 0;
        _tmp$3 = (idx$2 + 1 | 0) & self.capacity_mask;
        _tmp$4 = _curr_entry;
        continue;
      } else {
        _tmp$2 = psl + 1 | 0;
        _tmp$3 = (idx$2 + 1 | 0) & self.capacity_mask;
        continue;
      }
    }
  }
}
function _M0MP311moonbitlang4core7builtin3Map15set__with__hashGsRP311moonbitlang4core7builtin3MapGiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEEE(self, key, value, hash) {
  let _tmp$2 = 0;
  let _tmp$3 = hash & self.capacity_mask;
  while (true) {
    const psl = _tmp$2;
    const idx = _tmp$3;
    const _tmp$4 = self.entries;
    $bound_check(_tmp$4, idx);
    const _bind$4 = _tmp$4[idx];
    if (_bind$4 === undefined) {
      if (self.size >= self.grow_at) {
        _M0MP311moonbitlang4core7builtin3Map4growGsRP311moonbitlang4core7builtin3MapGiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEEE(self);
        _tmp$2 = 0;
        _tmp$3 = hash & self.capacity_mask;
        continue;
      }
      const _bind$5 = self.tail;
      const _bind$6 = undefined;
      const entry = { prev: _bind$5, next: _bind$6, psl: psl, hash: hash, key: key, value: value };
      _M0MP311moonbitlang4core7builtin3Map20add__entry__to__tailGsRP311moonbitlang4core7builtin3MapGiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEEE(self, idx, entry);
      return undefined;
    } else {
      const _Some = _bind$4;
      const _curr_entry = _Some;
      if (_curr_entry.hash === hash && _curr_entry.key === key) {
        _curr_entry.value = value;
        return undefined;
      }
      if (psl > _curr_entry.psl) {
        if (self.size >= self.grow_at) {
          _M0MP311moonbitlang4core7builtin3Map4growGsRP311moonbitlang4core7builtin3MapGiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEEE(self);
          _tmp$2 = 0;
          _tmp$3 = hash & self.capacity_mask;
          continue;
        }
        _M0MP311moonbitlang4core7builtin3Map10push__awayGsRP311moonbitlang4core7builtin3MapGiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEEE(self, idx, _curr_entry);
        const _bind$5 = self.tail;
        const _bind$6 = undefined;
        const entry = { prev: _bind$5, next: _bind$6, psl: psl, hash: hash, key: key, value: value };
        _M0MP311moonbitlang4core7builtin3Map20add__entry__to__tailGsRP311moonbitlang4core7builtin3MapGiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEEE(self, idx, entry);
        return undefined;
      }
      _tmp$2 = psl + 1 | 0;
      _tmp$3 = (idx + 1 | 0) & self.capacity_mask;
      continue;
    }
  }
}
function _M0MP311moonbitlang4core7builtin3Map15set__with__hashGiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEE(self, key, value, hash) {
  let _tmp$2 = 0;
  let _tmp$3 = hash & self.capacity_mask;
  while (true) {
    const psl = _tmp$2;
    const idx = _tmp$3;
    const _tmp$4 = self.entries;
    $bound_check(_tmp$4, idx);
    const _bind$4 = _tmp$4[idx];
    if (_bind$4 === undefined) {
      if (self.size >= self.grow_at) {
        _M0MP311moonbitlang4core7builtin3Map4growGiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEE(self);
        _tmp$2 = 0;
        _tmp$3 = hash & self.capacity_mask;
        continue;
      }
      const _bind$5 = self.tail;
      const _bind$6 = undefined;
      const entry = { prev: _bind$5, next: _bind$6, psl: psl, hash: hash, key: key, value: value };
      _M0MP311moonbitlang4core7builtin3Map20add__entry__to__tailGiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEE(self, idx, entry);
      return undefined;
    } else {
      const _Some = _bind$4;
      const _curr_entry = _Some;
      if (_curr_entry.hash === hash && _curr_entry.key === key) {
        _curr_entry.value = value;
        return undefined;
      }
      if (psl > _curr_entry.psl) {
        if (self.size >= self.grow_at) {
          _M0MP311moonbitlang4core7builtin3Map4growGiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEE(self);
          _tmp$2 = 0;
          _tmp$3 = hash & self.capacity_mask;
          continue;
        }
        _M0MP311moonbitlang4core7builtin3Map10push__awayGiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEE(self, idx, _curr_entry);
        const _bind$5 = self.tail;
        const _bind$6 = undefined;
        const entry = { prev: _bind$5, next: _bind$6, psl: psl, hash: hash, key: key, value: value };
        _M0MP311moonbitlang4core7builtin3Map20add__entry__to__tailGiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEE(self, idx, entry);
        return undefined;
      }
      _tmp$2 = psl + 1 | 0;
      _tmp$3 = (idx + 1 | 0) & self.capacity_mask;
      continue;
    }
  }
}
function _M0MP311moonbitlang4core7builtin3Map4growGsRP311moonbitlang4core7builtin3MapGiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEEE(self) {
  const old_head = self.head;
  const new_capacity = self.capacity << 1;
  self.entries = $make_array_len_and_init(new_capacity, undefined);
  self.capacity = new_capacity;
  self.capacity_mask = new_capacity - 1 | 0;
  self.grow_at = _M0FP311moonbitlang4core7builtin21calc__grow__threshold(self.capacity);
  self.size = 0;
  self.head = undefined;
  self.tail = -1;
  let _tmp$2 = old_head;
  while (true) {
    const _param = _tmp$2;
    if (_param === undefined) {
      return;
    } else {
      const _Some = _param;
      const _x = _Some;
      const _next = _x.next;
      const _key = _x.key;
      const _value = _x.value;
      const _hash = _x.hash;
      _M0MP311moonbitlang4core7builtin3Map15set__with__hashGsRP311moonbitlang4core7builtin3MapGiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEEE(self, _key, _value, _hash);
      _tmp$2 = _next;
      continue;
    }
  }
}
function _M0MP311moonbitlang4core7builtin3Map4growGiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEE(self) {
  const old_head = self.head;
  const new_capacity = self.capacity << 1;
  self.entries = $make_array_len_and_init(new_capacity, undefined);
  self.capacity = new_capacity;
  self.capacity_mask = new_capacity - 1 | 0;
  self.grow_at = _M0FP311moonbitlang4core7builtin21calc__grow__threshold(self.capacity);
  self.size = 0;
  self.head = undefined;
  self.tail = -1;
  let _tmp$2 = old_head;
  while (true) {
    const _param = _tmp$2;
    if (_param === undefined) {
      return;
    } else {
      const _Some = _param;
      const _x = _Some;
      const _next = _x.next;
      const _key = _x.key;
      const _value = _x.value;
      const _hash = _x.hash;
      _M0MP311moonbitlang4core7builtin3Map15set__with__hashGiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEE(self, _key, _value, _hash);
      _tmp$2 = _next;
      continue;
    }
  }
}
function _M0MP311moonbitlang4core7builtin3Map3setGsRP311moonbitlang4core7builtin3MapGiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEEE(self, key, value) {
  _M0MP311moonbitlang4core7builtin3Map15set__with__hashGsRP311moonbitlang4core7builtin3MapGiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEEE(self, key, value, _M0IP016_24default__implP311moonbitlang4core7builtin4Hash4hashGsE(key));
}
function _M0MP311moonbitlang4core7builtin3Map3setGiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEE(self, key, value) {
  _M0MP311moonbitlang4core7builtin3Map15set__with__hashGiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEE(self, key, value, _M0IP016_24default__implP311moonbitlang4core7builtin4Hash4hashGiE(key));
}
function _M0MP311moonbitlang4core7builtin3Map11from__arrayGsRP311moonbitlang4core7builtin3MapGiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEEE(arr) {
  const length = _M0MP311moonbitlang4core5array9ArrayView6lengthGUsRP311moonbitlang4core7builtin3MapGiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEEEE(arr);
  let capacity = _M0MP311moonbitlang4core3int3Int20next__power__of__two(length);
  if (length > _M0FP311moonbitlang4core7builtin21calc__grow__threshold(capacity)) {
    capacity = Math.imul(capacity, 2) | 0;
  }
  const m = _M0MP311moonbitlang4core7builtin3Map11new_2einnerGsRP311moonbitlang4core7builtin3MapGiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEEE(capacity);
  const _len = _M0MP311moonbitlang4core5array9ArrayView6lengthGUsRP311moonbitlang4core7builtin3MapGiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEEEE(arr);
  let _tmp$2 = 0;
  while (true) {
    const _i = _tmp$2;
    if (_i < _len) {
      const e = arr.buf[arr.start + _i | 0];
      _M0MP311moonbitlang4core7builtin3Map3setGsRP311moonbitlang4core7builtin3MapGiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEEE(m, e._0, e._1);
      _tmp$2 = _i + 1 | 0;
      continue;
    } else {
      break;
    }
  }
  return m;
}
function _M0MP311moonbitlang4core7builtin3Map11from__arrayGiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEE(arr) {
  const length = _M0MP311moonbitlang4core5array9ArrayView6lengthGUiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEEE(arr);
  let capacity = _M0MP311moonbitlang4core3int3Int20next__power__of__two(length);
  if (length > _M0FP311moonbitlang4core7builtin21calc__grow__threshold(capacity)) {
    capacity = Math.imul(capacity, 2) | 0;
  }
  const m = _M0MP311moonbitlang4core7builtin3Map11new_2einnerGiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEE(capacity);
  const _len = _M0MP311moonbitlang4core5array9ArrayView6lengthGUiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEEE(arr);
  let _tmp$2 = 0;
  while (true) {
    const _i = _tmp$2;
    if (_i < _len) {
      const e = arr.buf[arr.start + _i | 0];
      _M0MP311moonbitlang4core7builtin3Map3setGiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEE(m, e._0, e._1);
      _tmp$2 = _i + 1 | 0;
      continue;
    } else {
      break;
    }
  }
  return m;
}
function _M0MP311moonbitlang4core7builtin3Map3getGiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEE(self, key) {
  const hash = _M0IP016_24default__implP311moonbitlang4core7builtin4Hash4hashGiE(key);
  let _tmp$2 = 0;
  let _tmp$3 = hash & self.capacity_mask;
  while (true) {
    const i = _tmp$2;
    const idx = _tmp$3;
    const _tmp$4 = self.entries;
    $bound_check(_tmp$4, idx);
    const _bind$4 = _tmp$4[idx];
    if (_bind$4 === undefined) {
      return undefined;
    } else {
      const _Some = _bind$4;
      const _entry = _Some;
      if (_entry.hash === hash && _entry.key === key) {
        return _entry.value;
      }
      if (i > _entry.psl) {
        return undefined;
      }
      _tmp$2 = i + 1 | 0;
      _tmp$3 = (idx + 1 | 0) & self.capacity_mask;
      continue;
    }
  }
}
function _M0MP311moonbitlang4core7builtin3Map3getGsRP311moonbitlang4core7builtin3MapGiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEEE(self, key) {
  const hash = _M0IP016_24default__implP311moonbitlang4core7builtin4Hash4hashGsE(key);
  let _tmp$2 = 0;
  let _tmp$3 = hash & self.capacity_mask;
  while (true) {
    const i = _tmp$2;
    const idx = _tmp$3;
    const _tmp$4 = self.entries;
    $bound_check(_tmp$4, idx);
    const _bind$4 = _tmp$4[idx];
    if (_bind$4 === undefined) {
      return undefined;
    } else {
      const _Some = _bind$4;
      const _entry = _Some;
      if (_entry.hash === hash && _entry.key === key) {
        return _entry.value;
      }
      if (i > _entry.psl) {
        return undefined;
      }
      _tmp$2 = i + 1 | 0;
      _tmp$3 = (idx + 1 | 0) & self.capacity_mask;
      continue;
    }
  }
}
function _M0MP311moonbitlang4core6string6String20unsafe__charcode__at(self, idx) {
  return self.charCodeAt(idx);
}
function _M0MP311moonbitlang4core7builtin6Hasher15combine__string(self, value) {
  const _end2501 = value.length;
  let _tmp$2 = 0;
  while (true) {
    const i = _tmp$2;
    if (i < _end2501) {
      _M0MP311moonbitlang4core7builtin6Hasher13combine__uint(self, value.charCodeAt(i));
      _tmp$2 = i + 1 | 0;
      continue;
    } else {
      return;
    }
  }
}
function _M0IP311moonbitlang4core6string6StringP311moonbitlang4core7builtin4Hash13hash__combine(self, hasher) {
  _M0MP311moonbitlang4core7builtin6Hasher15combine__string(hasher, self);
}
function _M0IP311moonbitlang4core3int3IntP311moonbitlang4core7builtin4Hash13hash__combine(self, hasher) {
  _M0MP311moonbitlang4core7builtin6Hasher12combine__int(hasher, self);
}
function _M0FP311moonbitlang4core7builtin7printlnGsE(input) {
  console.log(_M0IP311moonbitlang4core6string6StringP311moonbitlang4core7builtin4Show10to__string(input));
}
function _M0IP311moonbitlang4core7builtin13SourceLocReprP311moonbitlang4core7builtin4Show6output(self, logger) {
  const pkg = self.pkg;
  const _data = _M0MP311moonbitlang4core6string10StringView4data(pkg);
  const _start = _M0MP311moonbitlang4core6string10StringView13start__offset(pkg);
  const _end = _start + _M0MP311moonbitlang4core6string10StringView6length(pkg) | 0;
  let _cursor = _start;
  let accept_state = -1;
  let match_end = -1;
  let match_tag_saver_0 = -1;
  let tag_0 = -1;
  let _bind$4;
  _L: {
    _L$2: {
      _L$3: while (true) {
        if (_cursor < _end) {
          const next_char = _M0MP311moonbitlang4core6string6String20unsafe__charcode__at(_data, _cursor);
          _cursor = _cursor + 1 | 0;
          if (next_char === 47) {
            _L$4: while (true) {
              tag_0 = _cursor;
              if (_cursor < _end) {
                const next_char$2 = _M0MP311moonbitlang4core6string6String20unsafe__charcode__at(_data, _cursor);
                _cursor = _cursor + 1 | 0;
                if (next_char$2 === 47) {
                  while (true) {
                    if (_cursor < _end) {
                      _M0MP311moonbitlang4core6string6String20unsafe__charcode__at(_data, _cursor);
                      _cursor = _cursor + 1 | 0;
                      continue;
                    } else {
                      match_tag_saver_0 = tag_0;
                      accept_state = 0;
                      match_end = _cursor;
                      break _L$2;
                    }
                  }
                } else {
                  continue;
                }
              } else {
                break _L$2;
              }
            }
          } else {
            continue;
          }
        } else {
          break _L$2;
        }
      }
      break _L;
    }
    if (accept_state === 0) {
      let package_name;
      let _try_err;
      _L$3: {
        _L$4: {
          const _bind$5 = _M0MP311moonbitlang4core6string6String3sub(_data, match_tag_saver_0 + 1 | 0, match_end);
          if (_bind$5.$tag === 1) {
            const _ok = _bind$5;
            package_name = _ok._0;
          } else {
            const _err = _bind$5;
            _try_err = _err._0;
            break _L$4;
          }
          break _L$3;
        }
        package_name = $panic();
      }
      let module_name;
      let _try_err$2;
      _L$4: {
        _L$5: {
          const _bind$5 = _M0MP311moonbitlang4core6string6String3sub(_data, _start, match_tag_saver_0);
          if (_bind$5.$tag === 1) {
            const _ok = _bind$5;
            module_name = _ok._0;
          } else {
            const _err = _bind$5;
            _try_err$2 = _err._0;
            break _L$5;
          }
          break _L$4;
        }
        module_name = $panic();
      }
      _bind$4 = { _0: module_name, _1: package_name };
    } else {
      _bind$4 = { _0: pkg, _1: undefined };
    }
  }
  const _module_name = _bind$4._0;
  const _package_name = _bind$4._1;
  if (_package_name === undefined) {
  } else {
    const _Some = _package_name;
    const _pkg_name = _Some;
    logger.method_table.method_2(logger.self, _pkg_name);
    logger.method_table.method_3(logger.self, 47);
  }
  logger.method_table.method_2(logger.self, self.filename);
  logger.method_table.method_3(logger.self, 58);
  logger.method_table.method_2(logger.self, self.start_line);
  logger.method_table.method_3(logger.self, 58);
  logger.method_table.method_2(logger.self, self.start_column);
  logger.method_table.method_3(logger.self, 45);
  logger.method_table.method_2(logger.self, self.end_line);
  logger.method_table.method_3(logger.self, 58);
  logger.method_table.method_2(logger.self, self.end_column);
  logger.method_table.method_3(logger.self, 64);
  logger.method_table.method_2(logger.self, _module_name);
}
function _M0IP311moonbitlang4core7builtin9SourceLocP311moonbitlang4core7builtin4Show6output(self, logger) {
  _M0IP311moonbitlang4core7builtin13SourceLocReprP311moonbitlang4core7builtin4Show6output(_M0MP311moonbitlang4core7builtin13SourceLocRepr5parse(self), logger);
}
function _M0MP311moonbitlang4core5array5Array4copyGsE(self) {
  return _M0MP311moonbitlang4core7builtin7JSArray4copy(self);
}
function _M0MP311moonbitlang4core5array5Array4copyGRP37astrata7astrata3lib8HandCardE(self) {
  return _M0MP311moonbitlang4core7builtin7JSArray4copy(self);
}
function _M0IP37astrata7astrata3lib5PhaseP311moonbitlang4core7builtin4Show6output(_x_216, _x_217) {
  switch (_x_216) {
    case 0: {
      _x_217.method_table.method_0(_x_217.self, "StartPhase");
      return;
    }
    case 1: {
      _x_217.method_table.method_0(_x_217.self, "EvolPhase");
      return;
    }
    case 2: {
      _x_217.method_table.method_0(_x_217.self, "ActionPhase");
      return;
    }
    case 3: {
      _x_217.method_table.method_0(_x_217.self, "AttackPhase");
      return;
    }
    default: {
      _x_217.method_table.method_0(_x_217.self, "EndPhase");
      return;
    }
  }
}
function _M0IP37astrata7astrata3lib15BattleFieldSlotP311moonbitlang4core7builtin4Show6output(_x_208, _x_209) {
  switch (_x_208) {
    case 0: {
      _x_209.method_table.method_0(_x_209.self, "OriginSlot");
      return;
    }
    case 1: {
      _x_209.method_table.method_0(_x_209.self, "FrontL");
      return;
    }
    case 2: {
      _x_209.method_table.method_0(_x_209.self, "FrontR");
      return;
    }
    case 3: {
      _x_209.method_table.method_0(_x_209.self, "BackL");
      return;
    }
    default: {
      _x_209.method_table.method_0(_x_209.self, "BackR");
      return;
    }
  }
}
function _M0IP37astrata7astrata3lib15BattleFieldSlotP311moonbitlang4core7builtin2Eq5equal(_x_204, _x_205) {
  switch (_x_204) {
    case 0: {
      if (_x_205 === 0) {
        return true;
      } else {
        return false;
      }
    }
    case 1: {
      if (_x_205 === 1) {
        return true;
      } else {
        return false;
      }
    }
    case 2: {
      if (_x_205 === 2) {
        return true;
      } else {
        return false;
      }
    }
    case 3: {
      if (_x_205 === 3) {
        return true;
      } else {
        return false;
      }
    }
    default: {
      if (_x_205 === 4) {
        return true;
      } else {
        return false;
      }
    }
  }
}
function _M0IP37astrata7astrata3lib9FieldZoneP311moonbitlang4core7builtin4Show6output(_x_196, _x_197) {
  switch (_x_196) {
    case 0: {
      _x_197.method_table.method_0(_x_197.self, "BattleZone");
      return;
    }
    case 1: {
      _x_197.method_table.method_0(_x_197.self, "AuraZone");
      return;
    }
    default: {
      _x_197.method_table.method_0(_x_197.self, "GuardZone");
      return;
    }
  }
}
function _M0IP37astrata7astrata3lib9FieldZoneP311moonbitlang4core7builtin2Eq5equal(_x_192, _x_193) {
  switch (_x_192) {
    case 0: {
      if (_x_193 === 0) {
        return true;
      } else {
        return false;
      }
    }
    case 1: {
      if (_x_193 === 1) {
        return true;
      } else {
        return false;
      }
    }
    default: {
      if (_x_193 === 2) {
        return true;
      } else {
        return false;
      }
    }
  }
}
function _M0IP37astrata7astrata3lib9SelectionP311moonbitlang4core7builtin4Show6output(_x_172, _x_173) {
  let _arg_178;
  let _arg_177;
  let _arg_179;
  _L: {
    let _arg_175;
    let _arg_176;
    _L$2: {
      let _arg_174;
      _L$3: {
        switch (_x_172.$tag) {
          case 0: {
            const _SideSelect = _x_172;
            const _$42$arg_174 = _SideSelect._0;
            _arg_174 = _$42$arg_174;
            break _L$3;
          }
          case 1: {
            const _FieldSelect = _x_172;
            const _$42$arg_175 = _FieldSelect._0;
            const _$42$arg_176 = _FieldSelect._1;
            _arg_175 = _$42$arg_175;
            _arg_176 = _$42$arg_176;
            break _L$2;
          }
          default: {
            const _SlotSelect = _x_172;
            const _$42$arg_177 = _SlotSelect._0;
            const _$42$arg_178 = _SlotSelect._1;
            const _$42$arg_179 = _SlotSelect._2;
            _arg_178 = _$42$arg_178;
            _arg_177 = _$42$arg_177;
            _arg_179 = _$42$arg_179;
            break _L;
          }
        }
      }
      _x_173.method_table.method_0(_x_173.self, "SideSelect(");
      _M0MP311moonbitlang4core7builtin6Logger13write__objectGRP37astrata7astrata3lib9FieldSideE(_x_173, _arg_174);
      _x_173.method_table.method_0(_x_173.self, ")");
      return;
    }
    _x_173.method_table.method_0(_x_173.self, "FieldSelect(");
    _M0MP311moonbitlang4core7builtin6Logger13write__objectGRP37astrata7astrata3lib9FieldSideE(_x_173, _arg_175);
    _x_173.method_table.method_0(_x_173.self, ", ");
    _M0MP311moonbitlang4core7builtin6Logger13write__objectGRP37astrata7astrata3lib9FieldZoneE(_x_173, _arg_176);
    _x_173.method_table.method_0(_x_173.self, ")");
    return;
  }
  _x_173.method_table.method_0(_x_173.self, "SlotSelect(");
  _M0MP311moonbitlang4core7builtin6Logger13write__objectGRP37astrata7astrata3lib9FieldSideE(_x_173, _arg_177);
  _x_173.method_table.method_0(_x_173.self, ", ");
  _M0MP311moonbitlang4core7builtin6Logger13write__objectGRP37astrata7astrata3lib9FieldZoneE(_x_173, _arg_178);
  _x_173.method_table.method_0(_x_173.self, ", ");
  _M0MP311moonbitlang4core7builtin6Logger13write__objectGRP37astrata7astrata3lib15BattleFieldSlotE(_x_173, _arg_179);
  _x_173.method_table.method_0(_x_173.self, ")");
}
function _M0IP37astrata7astrata3lib9SelectionP311moonbitlang4core7builtin2Eq5equal(_x_144, _x_145) {
  let _x2_154;
  let _x0_152;
  let _x1_153;
  let _y1_156;
  let _y0_155;
  let _y2_157;
  _L: {
    let _x1_149;
    let _x0_148;
    let _y0_150;
    let _y1_151;
    _L$2: {
      let _x0_146;
      let _y0_147;
      _L$3: {
        switch (_x_144.$tag) {
          case 0: {
            const _SideSelect = _x_144;
            const _$42$x0_146 = _SideSelect._0;
            if (_x_145.$tag === 0) {
              const _SideSelect$2 = _x_145;
              const _$42$y0_147 = _SideSelect$2._0;
              _x0_146 = _$42$x0_146;
              _y0_147 = _$42$y0_147;
              break _L$3;
            } else {
              return false;
            }
          }
          case 1: {
            const _FieldSelect = _x_144;
            const _$42$x0_148 = _FieldSelect._0;
            const _$42$x1_149 = _FieldSelect._1;
            if (_x_145.$tag === 1) {
              const _FieldSelect$2 = _x_145;
              const _$42$y0_150 = _FieldSelect$2._0;
              const _$42$y1_151 = _FieldSelect$2._1;
              _x1_149 = _$42$x1_149;
              _x0_148 = _$42$x0_148;
              _y0_150 = _$42$y0_150;
              _y1_151 = _$42$y1_151;
              break _L$2;
            } else {
              return false;
            }
          }
          default: {
            const _SlotSelect = _x_144;
            const _$42$x0_152 = _SlotSelect._0;
            const _$42$x1_153 = _SlotSelect._1;
            const _$42$x2_154 = _SlotSelect._2;
            if (_x_145.$tag === 2) {
              const _SlotSelect$2 = _x_145;
              const _$42$y0_155 = _SlotSelect$2._0;
              const _$42$y1_156 = _SlotSelect$2._1;
              const _$42$y2_157 = _SlotSelect$2._2;
              _x2_154 = _$42$x2_154;
              _x0_152 = _$42$x0_152;
              _x1_153 = _$42$x1_153;
              _y1_156 = _$42$y1_156;
              _y0_155 = _$42$y0_155;
              _y2_157 = _$42$y2_157;
              break _L;
            } else {
              return false;
            }
          }
        }
      }
      return _M0IP37astrata7astrata3lib9FieldSideP311moonbitlang4core7builtin2Eq5equal(_x0_146, _y0_147);
    }
    return _M0IP37astrata7astrata3lib9FieldSideP311moonbitlang4core7builtin2Eq5equal(_x0_148, _y0_150) && _M0IP37astrata7astrata3lib9FieldZoneP311moonbitlang4core7builtin2Eq5equal(_x1_149, _y1_151);
  }
  return _M0IP37astrata7astrata3lib9FieldSideP311moonbitlang4core7builtin2Eq5equal(_x0_152, _y0_155) && (_M0IP37astrata7astrata3lib9FieldZoneP311moonbitlang4core7builtin2Eq5equal(_x1_153, _y1_156) && _M0IP37astrata7astrata3lib15BattleFieldSlotP311moonbitlang4core7builtin2Eq5equal(_x2_154, _y2_157));
}
function _M0IP37astrata7astrata3lib9FieldSideP311moonbitlang4core7builtin4Show6output(_x_140, _x_141) {
  if (_x_140 === 0) {
    _x_141.method_table.method_0(_x_141.self, "Own");
    return;
  } else {
    _x_141.method_table.method_0(_x_141.self, "Rival");
    return;
  }
}
function _M0IP37astrata7astrata3lib9FieldSideP311moonbitlang4core7builtin2Eq5equal(_x_136, _x_137) {
  if (_x_136 === 0) {
    if (_x_137 === 0) {
      return true;
    } else {
      return false;
    }
  } else {
    if (_x_137 === 1) {
      return true;
    } else {
      return false;
    }
  }
}
function _M0FP37astrata7astrata3lib15draw__one__card(field) {
  if (field.stack.length === 0) {
    return field;
  }
  const card = _M0MP311moonbitlang4core5array5Array2atGRP37astrata7astrata3lib8HandCardE(field.stack, 0);
  const new_stack = [];
  const _start55 = 1;
  const _end56 = field.stack.length;
  let _tmp$2 = _start55;
  while (true) {
    const i = _tmp$2;
    if (i < _end56) {
      _M0MP311moonbitlang4core5array5Array4pushGRP37astrata7astrata3lib8HandCardE(new_stack, _M0MP311moonbitlang4core5array5Array2atGRP37astrata7astrata3lib8HandCardE(field.stack, i));
      _tmp$2 = i + 1 | 0;
      continue;
    } else {
      break;
    }
  }
  const new_hands = _M0MP311moonbitlang4core5array5Array4copyGRP37astrata7astrata3lib8HandCardE(field.hands);
  _M0MP311moonbitlang4core5array5Array4pushGRP37astrata7astrata3lib8HandCardE(new_hands, card);
  return { origin: field.origin, front_l: field.front_l, front_r: field.front_r, back_l: field.back_l, back_r: field.back_r, aura: field.aura, astra_records: field.astra_records, hands: new_hands, stack: new_stack };
}
function _M0FP37astrata7astrata3lib10field__for(bf, side) {
  if (side === 0) {
    return bf.own;
  } else {
    return bf.rival;
  }
}
function _M0FP37astrata7astrata3lib10set__field(bf, side, field) {
  if (side === 0) {
    return { own: field, rival: bf.rival };
  } else {
    return { own: bf.own, rival: field };
  }
}
function _M0FP37astrata7astrata3lib12start__phase(bf, side) {
  const field = _M0FP37astrata7astrata3lib10field__for(bf, side);
  const new_field = _M0FP37astrata7astrata3lib15draw__one__card(field);
  const new_bf = _M0FP37astrata7astrata3lib10set__field(bf, side, new_field);
  const log = [`Start Phase: ${_M0IP016_24default__implP311moonbitlang4core7builtin4Show10to__stringGRP37astrata7astrata3lib9FieldSideE(side)}`];
  return { _0: new_bf, _1: log };
}
function _M0FP37astrata7astrata3lib11evol__phase(bf, side) {
  return { _0: bf, _1: [`Evol Phase: ${_M0IP016_24default__implP311moonbitlang4core7builtin4Show10to__stringGRP37astrata7astrata3lib9FieldSideE(side)}`] };
}
function _M0FP37astrata7astrata3lib13action__phase(bf, side) {
  return { _0: bf, _1: [`Action Phase: ${_M0IP016_24default__implP311moonbitlang4core7builtin4Show10to__stringGRP37astrata7astrata3lib9FieldSideE(side)}`] };
}
function _M0FP37astrata7astrata3lib13attack__phase(bf, side) {
  return { _0: bf, _1: [`Attack Phase: ${_M0IP016_24default__implP311moonbitlang4core7builtin4Show10to__stringGRP37astrata7astrata3lib9FieldSideE(side)}`] };
}
function _M0FP37astrata7astrata3lib10end__phase(bf, side) {
  return { _0: bf, _1: [`End Phase: ${_M0IP016_24default__implP311moonbitlang4core7builtin4Show10to__stringGRP37astrata7astrata3lib9FieldSideE(side)}`] };
}
function _M0FP37astrata7astrata3lib12concat__logs(logs) {
  const result = [];
  const _arr = logs;
  const _len = _arr.length;
  let _tmp$2 = 0;
  while (true) {
    const _i = _tmp$2;
    if (_i < _len) {
      const segment = _arr[_i];
      const _arr$2 = segment;
      const _len$2 = _arr$2.length;
      let _tmp$3 = 0;
      while (true) {
        const _i$2 = _tmp$3;
        if (_i$2 < _len$2) {
          const entry = _arr$2[_i$2];
          _M0MP311moonbitlang4core5array5Array4pushGsE(result, entry);
          _tmp$3 = _i$2 + 1 | 0;
          continue;
        } else {
          break;
        }
      }
      _tmp$2 = _i + 1 | 0;
      continue;
    } else {
      break;
    }
  }
  return result;
}
function _M0FP37astrata7astrata3lib9run__turn(bf, side) {
  let bf1;
  let log1;
  _L: {
    const _bind$4 = _M0FP37astrata7astrata3lib12start__phase(bf, side);
    const _bf1 = _bind$4._0;
    const _log1 = _bind$4._1;
    bf1 = _bf1;
    log1 = _log1;
    break _L;
  }
  let bf2;
  let log2;
  _L$2: {
    const _bind$4 = _M0FP37astrata7astrata3lib11evol__phase(bf1, side);
    const _bf2 = _bind$4._0;
    const _log2 = _bind$4._1;
    bf2 = _bf2;
    log2 = _log2;
    break _L$2;
  }
  let bf3;
  let log3;
  _L$3: {
    const _bind$4 = _M0FP37astrata7astrata3lib13action__phase(bf2, side);
    const _bf3 = _bind$4._0;
    const _log3 = _bind$4._1;
    bf3 = _bf3;
    log3 = _log3;
    break _L$3;
  }
  let bf4;
  let log4;
  _L$4: {
    const _bind$4 = _M0FP37astrata7astrata3lib13attack__phase(bf3, side);
    const _bf4 = _bind$4._0;
    const _log4 = _bind$4._1;
    bf4 = _bf4;
    log4 = _log4;
    break _L$4;
  }
  let bf5;
  let log5;
  _L$5: {
    const _bind$4 = _M0FP37astrata7astrata3lib10end__phase(bf4, side);
    const _bf5 = _bind$4._0;
    const _log5 = _bind$4._1;
    bf5 = _bf5;
    log5 = _log5;
    break _L$5;
  }
  const all_logs = _M0FP37astrata7astrata3lib12concat__logs([log1, log2, log3, log4, log5]);
  return { _0: bf5, _1: all_logs };
}
function _M0FP37astrata7astrata3lib10next__turn(state) {
  const _bind$4 = state.turn;
  let next_side;
  if (_bind$4 === 0) {
    next_side = 1;
  } else {
    next_side = 0;
  }
  return { battlefield: state.battlefield, turn: next_side, phase: state.phase, log: state.log };
}
function _M0FP37astrata7astrata3lib11apply__turn(state) {
  let new_bf;
  let turn_logs;
  _L: {
    const _bind$4 = _M0FP37astrata7astrata3lib9run__turn(state.battlefield, state.turn);
    const _new_bf = _bind$4._0;
    const _turn_logs = _bind$4._1;
    new_bf = _new_bf;
    turn_logs = _turn_logs;
    break _L;
  }
  const new_log = _M0MP311moonbitlang4core5array5Array4copyGsE(state.log);
  const _arr = turn_logs;
  const _len = _arr.length;
  let _tmp$2 = 0;
  while (true) {
    const _i = _tmp$2;
    if (_i < _len) {
      const entry = _arr[_i];
      _M0MP311moonbitlang4core5array5Array4pushGsE(new_log, entry);
      _tmp$2 = _i + 1 | 0;
      continue;
    } else {
      break;
    }
  }
  return _M0FP37astrata7astrata3lib10next__turn({ battlefield: new_bf, turn: state.turn, phase: state.phase, log: new_log });
}
function _M0FP37astrata7astrata3lib11slot__value(selected, target, current, inst) {
  return _M0IP37astrata7astrata3lib15BattleFieldSlotP311moonbitlang4core7builtin2Eq5equal(selected, target) ? new $64$astrata$47$astrata$47$lib$46$SatelliteSlot$Assigned(inst) : current;
}
function _M0FP37astrata7astrata3lib17assign__satellite(field, inst, new_hands, slot) {
  return { origin: field.origin, front_l: _M0FP37astrata7astrata3lib11slot__value(slot, 1, field.front_l, inst), front_r: _M0FP37astrata7astrata3lib11slot__value(slot, 2, field.front_r, inst), back_l: _M0FP37astrata7astrata3lib11slot__value(slot, 3, field.back_l, inst), back_r: _M0FP37astrata7astrata3lib11slot__value(slot, 4, field.back_r, inst), aura: field.aura, astra_records: field.astra_records, hands: new_hands, stack: field.stack };
}
function _M0FP37astrata7astrata3lib15is__empty__slot(slot) {
  if (slot.$tag === 0) {
    return true;
  } else {
    return false;
  }
}
function _M0FP37astrata7astrata3lib27place__satellite__to__empty(field, inst) {
  const new_hands = [];
  const _start72 = 1;
  const _end73 = field.hands.length;
  let _tmp$2 = _start72;
  while (true) {
    const i = _tmp$2;
    if (i < _end73) {
      _M0MP311moonbitlang4core5array5Array4pushGRP37astrata7astrata3lib8HandCardE(new_hands, _M0MP311moonbitlang4core5array5Array2atGRP37astrata7astrata3lib8HandCardE(field.hands, i));
      _tmp$2 = i + 1 | 0;
      continue;
    } else {
      break;
    }
  }
  if (_M0FP37astrata7astrata3lib15is__empty__slot(field.front_l)) {
    return _M0FP37astrata7astrata3lib17assign__satellite(field, inst, new_hands, 1);
  }
  if (_M0FP37astrata7astrata3lib15is__empty__slot(field.front_r)) {
    return _M0FP37astrata7astrata3lib17assign__satellite(field, inst, new_hands, 2);
  }
  if (_M0FP37astrata7astrata3lib15is__empty__slot(field.back_l)) {
    return _M0FP37astrata7astrata3lib17assign__satellite(field, inst, new_hands, 3);
  }
  if (_M0FP37astrata7astrata3lib15is__empty__slot(field.back_r)) {
    return _M0FP37astrata7astrata3lib17assign__satellite(field, inst, new_hands, 4);
  }
  return field;
}
function _M0FP37astrata7astrata3lib10play__card(field) {
  if (field.hands.length === 0) {
    return field;
  }
  let inst;
  _L: {
    const _bind$4 = _M0MP311moonbitlang4core5array5Array2atGRP37astrata7astrata3lib8HandCardE(field.hands, 0);
    if (_bind$4.$tag === 0) {
      const _HSatellite = _bind$4;
      const _inst = _HSatellite._0;
      inst = _inst;
      break _L;
    } else {
      return field;
    }
  }
  return _M0FP37astrata7astrata3lib27place__satellite__to__empty(field, inst);
}
function _M0FP37astrata7astrata3lib22apply__play__own__card(state) {
  const new_own = _M0FP37astrata7astrata3lib10play__card(state.battlefield.own);
  return { battlefield: { own: new_own, rival: state.battlefield.rival }, turn: state.turn, phase: state.phase, log: state.log };
}
function _M0FP37astrata7astrata3lib23is__satellite__assigned(slot) {
  if (slot.$tag === 2) {
    return true;
  } else {
    return false;
  }
}
function _M0FP37astrata7astrata3lib12is__assigned(field, slot) {
  switch (slot) {
    case 0: {
      return true;
    }
    case 1: {
      return _M0FP37astrata7astrata3lib23is__satellite__assigned(field.front_l);
    }
    case 2: {
      return _M0FP37astrata7astrata3lib23is__satellite__assigned(field.front_r);
    }
    case 3: {
      return _M0FP37astrata7astrata3lib23is__satellite__assigned(field.back_l);
    }
    default: {
      return _M0FP37astrata7astrata3lib23is__satellite__assigned(field.back_r);
    }
  }
}
function _M0FP37astrata7astrata3lib15next__selection(sel) {
  _L: {
    let side;
    _L$2: {
      let side$2;
      _L$3: {
        switch (sel.$tag) {
          case 0: {
            const _SideSelect = sel;
            const _side = _SideSelect._0;
            side$2 = _side;
            break _L$3;
          }
          case 1: {
            const _FieldSelect = sel;
            const _side$2 = _FieldSelect._0;
            const _x = _FieldSelect._1;
            switch (_x) {
              case 0: {
                side = _side$2;
                break _L$2;
              }
              case 1: {
                break _L;
              }
              default: {
                break _L;
              }
            }
          }
          default: {
            return sel;
          }
        }
      }
      return new $64$astrata$47$astrata$47$lib$46$Selection$FieldSelect(side$2, 0);
    }
    return new $64$astrata$47$astrata$47$lib$46$Selection$SlotSelect(side, 0, 0);
  }
  return sel;
}
function _M0FP37astrata7astrata3lib15prev__selection(sel) {
  let zone;
  let side;
  _L: {
    let side$2;
    _L$2: {
      switch (sel.$tag) {
        case 0: {
          return sel;
        }
        case 1: {
          const _FieldSelect = sel;
          const _side = _FieldSelect._0;
          side$2 = _side;
          break _L$2;
        }
        default: {
          const _SlotSelect = sel;
          const _side$2 = _SlotSelect._0;
          const _zone = _SlotSelect._1;
          zone = _zone;
          side = _side$2;
          break _L;
        }
      }
    }
    return new $64$astrata$47$astrata$47$lib$46$Selection$SideSelect(side$2);
  }
  return new $64$astrata$47$astrata$47$lib$46$Selection$FieldSelect(side, zone);
}
function _M0FP37astrata7astrata3lib20find__assigned__from(field, side, zone, slot, step_fn) {
  let _tmp$2 = field;
  let _tmp$3 = side;
  let _tmp$4 = zone;
  let _tmp$5 = slot;
  let _tmp$6 = step_fn;
  while (true) {
    const field$2 = _tmp$2;
    const side$2 = _tmp$3;
    const zone$2 = _tmp$4;
    const slot$2 = _tmp$5;
    const step_fn$2 = _tmp$6;
    if (_M0FP37astrata7astrata3lib12is__assigned(field$2, slot$2)) {
      return new $64$astrata$47$astrata$47$lib$46$Selection$SlotSelect(side$2, zone$2, slot$2);
    } else {
      _tmp$5 = step_fn$2(slot$2);
      continue;
    }
  }
}
function _M0FP37astrata7astrata3lib20next__assigned__slot(bf, side, zone, current, step_fn) {
  const field = _M0FP37astrata7astrata3lib10field__for(bf, side);
  const candidate = step_fn(current);
  return _M0FP37astrata7astrata3lib20find__assigned__from(field, side, zone, candidate, step_fn);
}
function _M0FP37astrata7astrata3lib10next__side(side) {
  if (side === 0) {
    return 1;
  } else {
    return 0;
  }
}
function _M0FP37astrata7astrata3lib10next__slot(slot) {
  switch (slot) {
    case 0: {
      return 1;
    }
    case 1: {
      return 2;
    }
    case 2: {
      return 3;
    }
    case 3: {
      return 4;
    }
    default: {
      return 0;
    }
  }
}
function _M0FP37astrata7astrata3lib10next__zone(zone) {
  switch (zone) {
    case 0: {
      return 1;
    }
    case 1: {
      return 2;
    }
    default: {
      return 0;
    }
  }
}
function _M0FP37astrata7astrata3lib15next__in__level(bf, sel) {
  let zone;
  let side;
  let slot;
  _L: {
    let side$2;
    let zone$2;
    _L$2: {
      let side$3;
      _L$3: {
        switch (sel.$tag) {
          case 0: {
            const _SideSelect = sel;
            const _side = _SideSelect._0;
            side$3 = _side;
            break _L$3;
          }
          case 1: {
            const _FieldSelect = sel;
            const _side$2 = _FieldSelect._0;
            const _zone = _FieldSelect._1;
            side$2 = _side$2;
            zone$2 = _zone;
            break _L$2;
          }
          default: {
            const _SlotSelect = sel;
            const _side$3 = _SlotSelect._0;
            const _zone$2 = _SlotSelect._1;
            const _slot = _SlotSelect._2;
            zone = _zone$2;
            side = _side$3;
            slot = _slot;
            break _L;
          }
        }
      }
      return new $64$astrata$47$astrata$47$lib$46$Selection$SideSelect(_M0FP37astrata7astrata3lib10next__side(side$3));
    }
    return new $64$astrata$47$astrata$47$lib$46$Selection$FieldSelect(side$2, _M0FP37astrata7astrata3lib10next__zone(zone$2));
  }
  return _M0FP37astrata7astrata3lib20next__assigned__slot(bf, side, zone, slot, _M0FP37astrata7astrata3lib10next__slot);
}
function _M0FP37astrata7astrata3lib10prev__side(side) {
  return _M0FP37astrata7astrata3lib10next__side(side);
}
function _M0FP37astrata7astrata3lib10prev__slot(slot) {
  switch (slot) {
    case 0: {
      return 4;
    }
    case 1: {
      return 0;
    }
    case 2: {
      return 1;
    }
    case 3: {
      return 2;
    }
    default: {
      return 3;
    }
  }
}
function _M0FP37astrata7astrata3lib10prev__zone(zone) {
  switch (zone) {
    case 0: {
      return 2;
    }
    case 1: {
      return 0;
    }
    default: {
      return 1;
    }
  }
}
function _M0FP37astrata7astrata3lib15prev__in__level(bf, sel) {
  let zone;
  let side;
  let slot;
  _L: {
    let side$2;
    let zone$2;
    _L$2: {
      let side$3;
      _L$3: {
        switch (sel.$tag) {
          case 0: {
            const _SideSelect = sel;
            const _side = _SideSelect._0;
            side$3 = _side;
            break _L$3;
          }
          case 1: {
            const _FieldSelect = sel;
            const _side$2 = _FieldSelect._0;
            const _zone = _FieldSelect._1;
            side$2 = _side$2;
            zone$2 = _zone;
            break _L$2;
          }
          default: {
            const _SlotSelect = sel;
            const _side$3 = _SlotSelect._0;
            const _zone$2 = _SlotSelect._1;
            const _slot = _SlotSelect._2;
            zone = _zone$2;
            side = _side$3;
            slot = _slot;
            break _L;
          }
        }
      }
      return new $64$astrata$47$astrata$47$lib$46$Selection$SideSelect(_M0FP37astrata7astrata3lib10prev__side(side$3));
    }
    return new $64$astrata$47$astrata$47$lib$46$Selection$FieldSelect(side$2, _M0FP37astrata7astrata3lib10prev__zone(zone$2));
  }
  return _M0FP37astrata7astrata3lib20next__assigned__slot(bf, side, zone, slot, _M0FP37astrata7astrata3lib10prev__slot);
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
function _M0FP37astrata7astrata3lib20initial__game__state() {
  return { battlefield: _M0FP37astrata7astrata3lib22initial__battle__field, turn: 0, phase: 0, log: [] };
}
function _M0FP36mizchi6kagura4core26new__input__snapshot__full(cursor_x, cursor_y, wheel_x, wheel_y, pressed_keys, pressed_mouse_buttons, touches, gamepads) {
  return { cursor_x: cursor_x, cursor_y: cursor_y, wheel_x: wheel_x, wheel_y: wheel_y, pressed_keys: pressed_keys, pressed_mouse_buttons: pressed_mouse_buttons, touches: touches, gamepads: gamepads };
}
function _M0FP36mizchi6kagura4core20new__input__snapshot(cursor_x, cursor_y, wheel_x, wheel_y, pressed_keys) {
  return _M0FP36mizchi6kagura4core26new__input__snapshot__full(cursor_x, cursor_y, wheel_x, wheel_y, pressed_keys, [], [], []);
}
function _M0FP46mizchi6kagura4core9inpututil13contains__key(keys, key) {
  const found = { val: false };
  const _arr = keys;
  const _len = _arr.length;
  let _tmp$2 = 0;
  while (true) {
    const _i = _tmp$2;
    if (_i < _len) {
      const current = _arr[_i];
      if (current === key) {
        found.val = true;
      }
      _tmp$2 = _i + 1 | 0;
      continue;
    } else {
      break;
    }
  }
  return found.val;
}
function _M0FP46mizchi6kagura4core9inpututil21normalize__touch__ids(snapshot) {
  const out = [];
  const _arr = snapshot.touches;
  const _len = _arr.length;
  let _tmp$2 = 0;
  while (true) {
    const _i = _tmp$2;
    if (_i < _len) {
      const touch = _arr[_i];
      if (!_M0FP46mizchi6kagura4core9inpututil13contains__key(out, touch.id)) {
        _M0MP311moonbitlang4core5array5Array4pushGiE(out, touch.id);
      }
      _tmp$2 = _i + 1 | 0;
      continue;
    } else {
      break;
    }
  }
  return out;
}
function _M0FP46mizchi6kagura4core9inpututil20duration__for__touch(entries, id) {
  const out = { val: 0 };
  const _arr = entries;
  const _len = _arr.length;
  let _tmp$2 = 0;
  while (true) {
    const _i = _tmp$2;
    if (_i < _len) {
      const entry = _arr[_i];
      if (entry.id === id) {
        out.val = entry.duration;
      }
      _tmp$2 = _i + 1 | 0;
      continue;
    } else {
      break;
    }
  }
  return out.val;
}
function _M0FP46mizchi6kagura4core9inpututil24new__touch__input__state() {
  return { prev_touch_ids: [], touch_ids: [], just_pressed_touch_ids: [], just_released_touch_ids: [], durations: [] };
}
function _M0FP46mizchi6kagura4core9inpututil27update__touch__input__state(state, snapshot) {
  const prev_ids = state.touch_ids;
  const next_ids = _M0FP46mizchi6kagura4core9inpututil21normalize__touch__ids(snapshot);
  const just_pressed = [];
  const _arr = next_ids;
  const _len = _arr.length;
  let _tmp$2 = 0;
  while (true) {
    const _i = _tmp$2;
    if (_i < _len) {
      const id = _arr[_i];
      if (!_M0FP46mizchi6kagura4core9inpututil13contains__key(prev_ids, id)) {
        _M0MP311moonbitlang4core5array5Array4pushGiE(just_pressed, id);
      }
      _tmp$2 = _i + 1 | 0;
      continue;
    } else {
      break;
    }
  }
  const just_released = [];
  const _arr$2 = prev_ids;
  const _len$2 = _arr$2.length;
  let _tmp$3 = 0;
  while (true) {
    const _i = _tmp$3;
    if (_i < _len$2) {
      const id = _arr$2[_i];
      if (!_M0FP46mizchi6kagura4core9inpututil13contains__key(next_ids, id)) {
        _M0MP311moonbitlang4core5array5Array4pushGiE(just_released, id);
      }
      _tmp$3 = _i + 1 | 0;
      continue;
    } else {
      break;
    }
  }
  const next_durations = [];
  const _arr$3 = next_ids;
  const _len$3 = _arr$3.length;
  let _tmp$4 = 0;
  while (true) {
    const _i = _tmp$4;
    if (_i < _len$3) {
      const id = _arr$3[_i];
      const duration = _M0FP46mizchi6kagura4core9inpututil13contains__key(prev_ids, id) ? _M0FP46mizchi6kagura4core9inpututil20duration__for__touch(state.durations, id) + 1 | 0 : 1;
      _M0MP311moonbitlang4core5array5Array4pushGRP46mizchi6kagura4core9inpututil18TouchDurationEntryE(next_durations, { id: id, duration: duration });
      _tmp$4 = _i + 1 | 0;
      continue;
    } else {
      break;
    }
  }
  state.prev_touch_ids = prev_ids;
  state.touch_ids = next_ids;
  state.just_pressed_touch_ids = just_pressed;
  state.just_released_touch_ids = just_released;
  state.durations = next_durations;
}
function _M0FP46mizchi6kagura4core9inpututil24normalize__pressed__keys(keys) {
  const out = [];
  const _arr = keys;
  const _len = _arr.length;
  let _tmp$2 = 0;
  while (true) {
    const _i = _tmp$2;
    if (_i < _len) {
      const key = _arr[_i];
      if (!_M0FP46mizchi6kagura4core9inpututil13contains__key(out, key)) {
        _M0MP311moonbitlang4core5array5Array4pushGiE(out, key);
      }
      _tmp$2 = _i + 1 | 0;
      continue;
    } else {
      break;
    }
  }
  return out;
}
function _M0FP46mizchi6kagura4core9inpututil18duration__for__key(entries, key) {
  const out = { val: 0 };
  const _arr = entries;
  const _len = _arr.length;
  let _tmp$2 = 0;
  while (true) {
    const _i = _tmp$2;
    if (_i < _len) {
      const entry = _arr[_i];
      if (entry.key === key) {
        out.val = entry.duration;
      }
      _tmp$2 = _i + 1 | 0;
      continue;
    } else {
      break;
    }
  }
  return out.val;
}
function _M0FP46mizchi6kagura4core9inpututil22new__key__input__state() {
  return { prev_pressed_keys: [], pressed_keys: [], just_pressed_keys: [], just_released_keys: [], durations: [] };
}
function _M0FP46mizchi6kagura4core9inpututil28duration__for__mouse__button(entries, button) {
  const out = { val: 0 };
  const _arr = entries;
  const _len = _arr.length;
  let _tmp$2 = 0;
  while (true) {
    const _i = _tmp$2;
    if (_i < _len) {
      const entry = _arr[_i];
      if (entry.button === button) {
        out.val = entry.duration;
      }
      _tmp$2 = _i + 1 | 0;
      continue;
    } else {
      break;
    }
  }
  return out.val;
}
function _M0FP46mizchi6kagura4core9inpututil24new__mouse__input__state() {
  return { prev_pressed_buttons: [], pressed_buttons: [], just_pressed_buttons: [], just_released_buttons: [], durations: [] };
}
function _M0FP46mizchi6kagura4core9inpututil25update__key__input__state(state, snapshot) {
  const prev_pressed = state.pressed_keys;
  const next_pressed = _M0FP46mizchi6kagura4core9inpututil24normalize__pressed__keys(snapshot.pressed_keys);
  const just_pressed = [];
  const _arr = next_pressed;
  const _len = _arr.length;
  let _tmp$2 = 0;
  while (true) {
    const _i = _tmp$2;
    if (_i < _len) {
      const key = _arr[_i];
      if (!_M0FP46mizchi6kagura4core9inpututil13contains__key(prev_pressed, key)) {
        _M0MP311moonbitlang4core5array5Array4pushGiE(just_pressed, key);
      }
      _tmp$2 = _i + 1 | 0;
      continue;
    } else {
      break;
    }
  }
  const just_released = [];
  const _arr$2 = prev_pressed;
  const _len$2 = _arr$2.length;
  let _tmp$3 = 0;
  while (true) {
    const _i = _tmp$3;
    if (_i < _len$2) {
      const key = _arr$2[_i];
      if (!_M0FP46mizchi6kagura4core9inpututil13contains__key(next_pressed, key)) {
        _M0MP311moonbitlang4core5array5Array4pushGiE(just_released, key);
      }
      _tmp$3 = _i + 1 | 0;
      continue;
    } else {
      break;
    }
  }
  const next_durations = [];
  const _arr$3 = next_pressed;
  const _len$3 = _arr$3.length;
  let _tmp$4 = 0;
  while (true) {
    const _i = _tmp$4;
    if (_i < _len$3) {
      const key = _arr$3[_i];
      const duration = _M0FP46mizchi6kagura4core9inpututil13contains__key(prev_pressed, key) ? _M0FP46mizchi6kagura4core9inpututil18duration__for__key(state.durations, key) + 1 | 0 : 1;
      _M0MP311moonbitlang4core5array5Array4pushGRP46mizchi6kagura4core9inpututil16KeyDurationEntryE(next_durations, { key: key, duration: duration });
      _tmp$4 = _i + 1 | 0;
      continue;
    } else {
      break;
    }
  }
  state.prev_pressed_keys = prev_pressed;
  state.pressed_keys = next_pressed;
  state.just_pressed_keys = just_pressed;
  state.just_released_keys = just_released;
  state.durations = next_durations;
}
function _M0FP46mizchi6kagura4core9inpututil27update__mouse__input__state(state, snapshot) {
  const prev_pressed = state.pressed_buttons;
  const next_pressed = _M0FP46mizchi6kagura4core9inpututil24normalize__pressed__keys(snapshot.pressed_mouse_buttons);
  const just_pressed = [];
  const _arr = next_pressed;
  const _len = _arr.length;
  let _tmp$2 = 0;
  while (true) {
    const _i = _tmp$2;
    if (_i < _len) {
      const button = _arr[_i];
      if (!_M0FP46mizchi6kagura4core9inpututil13contains__key(prev_pressed, button)) {
        _M0MP311moonbitlang4core5array5Array4pushGiE(just_pressed, button);
      }
      _tmp$2 = _i + 1 | 0;
      continue;
    } else {
      break;
    }
  }
  const just_released = [];
  const _arr$2 = prev_pressed;
  const _len$2 = _arr$2.length;
  let _tmp$3 = 0;
  while (true) {
    const _i = _tmp$3;
    if (_i < _len$2) {
      const button = _arr$2[_i];
      if (!_M0FP46mizchi6kagura4core9inpututil13contains__key(next_pressed, button)) {
        _M0MP311moonbitlang4core5array5Array4pushGiE(just_released, button);
      }
      _tmp$3 = _i + 1 | 0;
      continue;
    } else {
      break;
    }
  }
  const next_durations = [];
  const _arr$3 = next_pressed;
  const _len$3 = _arr$3.length;
  let _tmp$4 = 0;
  while (true) {
    const _i = _tmp$4;
    if (_i < _len$3) {
      const button = _arr$3[_i];
      const duration = _M0FP46mizchi6kagura4core9inpututil13contains__key(prev_pressed, button) ? _M0FP46mizchi6kagura4core9inpututil28duration__for__mouse__button(state.durations, button) + 1 | 0 : 1;
      _M0MP311moonbitlang4core5array5Array4pushGRP46mizchi6kagura4core9inpututil24MouseButtonDurationEntryE(next_durations, { button: button, duration: duration });
      _tmp$4 = _i + 1 | 0;
      continue;
    } else {
      break;
    }
  }
  state.prev_pressed_buttons = prev_pressed;
  state.pressed_buttons = next_pressed;
  state.just_pressed_buttons = just_pressed;
  state.just_released_buttons = just_released;
  state.durations = next_durations;
}
function _M0FP46mizchi6kagura4core9inpututil22is__key__just__pressed(state, key) {
  return _M0FP46mizchi6kagura4core9inpututil13contains__key(state.just_pressed_keys, key);
}
function _M0FP46mizchi6kagura4core9inpututil18new__input__helper() {
  return { key_state: _M0FP46mizchi6kagura4core9inpututil22new__key__input__state(), mouse_state: _M0FP46mizchi6kagura4core9inpututil24new__mouse__input__state(), touch_state: _M0FP46mizchi6kagura4core9inpututil24new__touch__input__state() };
}
function _M0FP46mizchi6kagura4core9inpututil21update__input__helper(helper, snapshot) {
  _M0FP46mizchi6kagura4core9inpututil25update__key__input__state(helper.key_state, snapshot);
  _M0FP46mizchi6kagura4core9inpututil27update__mouse__input__state(helper.mouse_state, snapshot);
  _M0FP46mizchi6kagura4core9inpututil27update__touch__input__state(helper.touch_state, snapshot);
}
function _M0FP46mizchi6kagura4core9inpututil26is__confirm__just__pressed(helper) {
  return _M0FP46mizchi6kagura4core9inpututil22is__key__just__pressed(helper.key_state, _M0FP46mizchi6kagura4core9inpututil10key__space) || (_M0FP46mizchi6kagura4core9inpututil22is__key__just__pressed(helper.key_state, _M0FP46mizchi6kagura4core9inpututil15key__enter__web) || _M0FP46mizchi6kagura4core9inpututil22is__key__just__pressed(helper.key_state, _M0FP46mizchi6kagura4core9inpututil16key__enter__glfw));
}
function _M0FP46mizchi6kagura4core9inpututil21is__up__just__pressed(helper) {
  return _M0FP46mizchi6kagura4core9inpututil22is__key__just__pressed(helper.key_state, _M0FP46mizchi6kagura4core9inpututil6key__w) || (_M0FP46mizchi6kagura4core9inpututil22is__key__just__pressed(helper.key_state, _M0FP46mizchi6kagura4core9inpututil10key__comma) || (_M0FP46mizchi6kagura4core9inpututil22is__key__just__pressed(helper.key_state, _M0FP46mizchi6kagura4core9inpututil12key__up__web) || _M0FP46mizchi6kagura4core9inpututil22is__key__just__pressed(helper.key_state, _M0FP46mizchi6kagura4core9inpututil13key__up__glfw)));
}
function _M0FP46mizchi6kagura4core9inpututil23is__down__just__pressed(helper) {
  return _M0FP46mizchi6kagura4core9inpututil22is__key__just__pressed(helper.key_state, _M0FP46mizchi6kagura4core9inpututil6key__s) || (_M0FP46mizchi6kagura4core9inpututil22is__key__just__pressed(helper.key_state, _M0FP46mizchi6kagura4core9inpututil6key__o) || (_M0FP46mizchi6kagura4core9inpututil22is__key__just__pressed(helper.key_state, _M0FP46mizchi6kagura4core9inpututil14key__down__web) || _M0FP46mizchi6kagura4core9inpututil22is__key__just__pressed(helper.key_state, _M0FP46mizchi6kagura4core9inpututil15key__down__glfw)));
}
function _M0FP37astrata7astrata4main18is__slot__selected(selection, side, zone, slot) {
  let z;
  let s;
  let sl;
  _L: {
    if (selection.$tag === 2) {
      const _SlotSelect = selection;
      const _s = _SlotSelect._0;
      const _z = _SlotSelect._1;
      const _sl = _SlotSelect._2;
      z = _z;
      s = _s;
      sl = _sl;
      break _L;
    } else {
      return false;
    }
  }
  return _M0IP37astrata7astrata3lib9FieldSideP311moonbitlang4core7builtin2Eq5equal(s, side) && (_M0IP37astrata7astrata3lib9FieldZoneP311moonbitlang4core7builtin2Eq5equal(z, zone) && _M0IP37astrata7astrata3lib15BattleFieldSlotP311moonbitlang4core7builtin2Eq5equal(sl, slot));
}
function _M0MP37astrata7astrata4main4Game3new() {
  return { state: _M0FP37astrata7astrata3lib20initial__game__state(), selection: new $64$astrata$47$astrata$47$lib$46$Selection$SideSelect(0), input: _M0FP46mizchi6kagura4core9inpututil18new__input__helper() };
}
function _M0FP37astrata7astrata4main53____test__67616d655f766965775f7762746573742e6d6274__0() {
  const game = _M0MP37astrata7astrata4main4Game3new();
  const _bind$4 = _M0FP311moonbitlang4core7builtin10assert__eqGRP37astrata7astrata3lib9SelectionE(game.selection, new $64$astrata$47$astrata$47$lib$46$Selection$SideSelect(0), undefined, "@astrata/astrata/main:game_view_wbtest.mbt:4:3-4:55");
  if (_bind$4.$tag === 1) {
    const _ok = _bind$4;
    _ok._0;
  } else {
    return _bind$4;
  }
  const _bind$5 = _M0FP311moonbitlang4core7builtin10assert__eqGRP37astrata7astrata3lib9FieldSideE(game.state.turn, 0, undefined, "@astrata/astrata/main:game_view_wbtest.mbt:5:3-5:39");
  if (_bind$5.$tag === 1) {
    const _ok = _bind$5;
    _ok._0;
  } else {
    return _bind$5;
  }
  const _bind$6 = _M0FP311moonbitlang4core7builtin10assert__eqGsE(_M0IP016_24default__implP311moonbitlang4core7builtin4Show10to__stringGRP37astrata7astrata3lib5PhaseE(game.state.phase), "StartPhase", undefined, "@astrata/astrata/main:game_view_wbtest.mbt:6:3-6:49");
  if (_bind$6.$tag === 1) {
    const _ok = _bind$6;
    _ok._0;
  } else {
    return _bind$6;
  }
  return _M0FP311moonbitlang4core7builtin10assert__eqGiE(game.state.battlefield.own.hands.length, 5, undefined, "@astrata/astrata/main:game_view_wbtest.mbt:7:3-7:58");
}
function _M0FP37astrata7astrata4main53____test__67616d655f766965775f7762746573742e6d6274__1() {
  const selection = new $64$astrata$47$astrata$47$lib$46$Selection$SlotSelect(0, 0, 1);
  return _M0FP311moonbitlang4core7builtin10assert__eqGbE(_M0FP37astrata7astrata4main18is__slot__selected(selection, 0, 0, 1), true, undefined, "@astrata/astrata/main:game_view_wbtest.mbt:13:3-16:4");
}
function _M0FP37astrata7astrata4main53____test__67616d655f766965775f7762746573742e6d6274__2() {
  const selection = new $64$astrata$47$astrata$47$lib$46$Selection$SlotSelect(0, 0, 1);
  return _M0FP311moonbitlang4core7builtin10assert__eqGbE(_M0FP37astrata7astrata4main18is__slot__selected(selection, 0, 2, 1), false, undefined, "@astrata/astrata/main:game_view_wbtest.mbt:22:3-25:4");
}
function _M0FP37astrata7astrata4main53____test__67616d655f766965775f7762746573742e6d6274__3() {
  const selection = new $64$astrata$47$astrata$47$lib$46$Selection$SlotSelect(0, 0, 1);
  return _M0FP311moonbitlang4core7builtin10assert__eqGbE(_M0FP37astrata7astrata4main18is__slot__selected(selection, 1, 0, 1), false, undefined, "@astrata/astrata/main:game_view_wbtest.mbt:31:3-34:4");
}
function _M0FP37astrata7astrata4main53____test__67616d655f766965775f7762746573742e6d6274__4() {
  return _M0FP311moonbitlang4core7builtin10assert__eqGbE(_M0FP37astrata7astrata4main18is__slot__selected(new $64$astrata$47$astrata$47$lib$46$Selection$FieldSelect(0, 0), 0, 0, 1), false, undefined, "@astrata/astrata/main:game_view_wbtest.mbt:39:3-47:4");
}
function _M0FP37astrata7astrata4main16input__with__key(key) {
  return _M0FP36mizchi6kagura4core20new__input__snapshot(0, 0, 0, 0, [key]);
}
function _M0FP37astrata7astrata4main31assigned__id__for__game__update(slot) {
  let inst;
  _L: {
    if (slot.$tag === 2) {
      const _Assigned = slot;
      const _inst = _Assigned._0;
      inst = _inst;
      break _L;
    } else {
      return _M0FP311moonbitlang4core7builtin5abortGiE("expected assigned satellite slot", "@astrata/astrata/main:game_update_wbtest.mbt:10:10-10:51");
    }
  }
  return inst.id;
}
function _M0IP37astrata7astrata4main4GameP36mizchi6kagura5scene9SceneGame12update__game(self, input) {
  _M0FP46mizchi6kagura4core9inpututil21update__input__helper(self.input, input);
  const bf = self.state.battlefield;
  const sel = self.selection;
  if (_M0FP46mizchi6kagura4core9inpututil22is__key__just__pressed(self.input.key_state, _M0FP46mizchi6kagura4core9inpututil14key__left__web)) {
    self.selection = _M0FP37astrata7astrata3lib15prev__selection(sel);
  }
  if (_M0FP46mizchi6kagura4core9inpututil22is__key__just__pressed(self.input.key_state, _M0FP46mizchi6kagura4core9inpututil15key__right__web)) {
    self.selection = _M0FP37astrata7astrata3lib15next__selection(sel);
  }
  if (_M0FP46mizchi6kagura4core9inpututil21is__up__just__pressed(self.input)) {
    self.selection = _M0FP37astrata7astrata3lib15prev__in__level(bf, sel);
  }
  if (_M0FP46mizchi6kagura4core9inpututil23is__down__just__pressed(self.input)) {
    self.selection = _M0FP37astrata7astrata3lib15next__in__level(bf, sel);
  }
  if (_M0FP46mizchi6kagura4core9inpututil26is__confirm__just__pressed(self.input)) {
    self.state = _M0FP37astrata7astrata3lib22apply__play__own__card(self.state);
  }
  if (_M0FP46mizchi6kagura4core9inpututil22is__key__just__pressed(self.input.key_state, _M0FP46mizchi6kagura4core9inpututil6key__e)) {
    self.state = _M0FP37astrata7astrata3lib11apply__turn(self.state);
    return;
  } else {
    return;
  }
}
function _M0FP37astrata7astrata4main57____test__67616d655f7570646174655f7762746573742e6d6274__0() {
  const game = _M0MP37astrata7astrata4main4Game3new();
  _M0IP37astrata7astrata4main4GameP36mizchi6kagura5scene9SceneGame12update__game(game, _M0FP37astrata7astrata4main16input__with__key(_M0FP46mizchi6kagura4core9inpututil15key__right__web));
  return _M0FP311moonbitlang4core7builtin10assert__eqGRP37astrata7astrata3lib9SelectionE(game.selection, new $64$astrata$47$astrata$47$lib$46$Selection$FieldSelect(0, 0), undefined, "@astrata/astrata/main:game_update_wbtest.mbt:18:3-18:73");
}
function _M0FP37astrata7astrata4main57____test__67616d655f7570646174655f7762746573742e6d6274__1() {
  const game = _M0MP37astrata7astrata4main4Game3new();
  game.selection = new $64$astrata$47$astrata$47$lib$46$Selection$SlotSelect(0, 0, 0);
  _M0IP37astrata7astrata4main4GameP36mizchi6kagura5scene9SceneGame12update__game(game, _M0FP37astrata7astrata4main16input__with__key(_M0FP46mizchi6kagura4core9inpututil14key__left__web));
  return _M0FP311moonbitlang4core7builtin10assert__eqGRP37astrata7astrata3lib9SelectionE(game.selection, new $64$astrata$47$astrata$47$lib$46$Selection$FieldSelect(0, 0), undefined, "@astrata/astrata/main:game_update_wbtest.mbt:26:3-26:73");
}
function _M0FP37astrata7astrata4main57____test__67616d655f7570646174655f7762746573742e6d6274__2() {
  const game = _M0MP37astrata7astrata4main4Game3new();
  _M0IP37astrata7astrata4main4GameP36mizchi6kagura5scene9SceneGame12update__game(game, _M0FP37astrata7astrata4main16input__with__key(_M0FP46mizchi6kagura4core9inpututil12key__up__web));
  return _M0FP311moonbitlang4core7builtin10assert__eqGRP37astrata7astrata3lib9SelectionE(game.selection, new $64$astrata$47$astrata$47$lib$46$Selection$SideSelect(1), undefined, "@astrata/astrata/main:game_update_wbtest.mbt:33:3-33:57");
}
function _M0FP37astrata7astrata4main57____test__67616d655f7570646174655f7762746573742e6d6274__3() {
  const game = _M0MP37astrata7astrata4main4Game3new();
  _M0IP37astrata7astrata4main4GameP36mizchi6kagura5scene9SceneGame12update__game(game, _M0FP37astrata7astrata4main16input__with__key(_M0FP46mizchi6kagura4core9inpututil14key__down__web));
  return _M0FP311moonbitlang4core7builtin10assert__eqGRP37astrata7astrata3lib9SelectionE(game.selection, new $64$astrata$47$astrata$47$lib$46$Selection$SideSelect(1), undefined, "@astrata/astrata/main:game_update_wbtest.mbt:40:3-40:57");
}
function _M0FP37astrata7astrata4main57____test__67616d655f7570646174655f7762746573742e6d6274__4() {
  const game = _M0MP37astrata7astrata4main4Game3new();
  const hand_count = game.state.battlefield.own.hands.length;
  _M0IP37astrata7astrata4main4GameP36mizchi6kagura5scene9SceneGame12update__game(game, _M0FP37astrata7astrata4main16input__with__key(_M0FP46mizchi6kagura4core9inpututil10key__space));
  const _bind$4 = _M0FP311moonbitlang4core7builtin10assert__eqGiE(_M0FP37astrata7astrata4main31assigned__id__for__game__update(game.state.battlefield.own.front_l), 101, undefined, "@astrata/astrata/main:game_update_wbtest.mbt:48:3-51:4");
  if (_bind$4.$tag === 1) {
    const _ok = _bind$4;
    _ok._0;
  } else {
    return _bind$4;
  }
  return _M0FP311moonbitlang4core7builtin10assert__eqGiE(game.state.battlefield.own.hands.length, hand_count - 1 | 0, undefined, "@astrata/astrata/main:game_update_wbtest.mbt:52:3-52:71");
}
function _M0FP37astrata7astrata4main57____test__67616d655f7570646174655f7762746573742e6d6274__5() {
  const game = _M0MP37astrata7astrata4main4Game3new();
  _M0IP37astrata7astrata4main4GameP36mizchi6kagura5scene9SceneGame12update__game(game, _M0FP37astrata7astrata4main16input__with__key(_M0FP46mizchi6kagura4core9inpututil15key__enter__web));
  return _M0FP311moonbitlang4core7builtin10assert__eqGiE(_M0FP37astrata7astrata4main31assigned__id__for__game__update(game.state.battlefield.own.front_l), 101, undefined, "@astrata/astrata/main:game_update_wbtest.mbt:59:3-62:4");
}
function _M0FP37astrata7astrata4main57____test__67616d655f7570646174655f7762746573742e6d6274__6() {
  const game = _M0MP37astrata7astrata4main4Game3new();
  _M0IP37astrata7astrata4main4GameP36mizchi6kagura5scene9SceneGame12update__game(game, _M0FP37astrata7astrata4main16input__with__key(_M0FP46mizchi6kagura4core9inpututil6key__e));
  const _bind$4 = _M0FP311moonbitlang4core7builtin10assert__eqGRP37astrata7astrata3lib9FieldSideE(game.state.turn, 1, undefined, "@astrata/astrata/main:game_update_wbtest.mbt:69:3-69:41");
  if (_bind$4.$tag === 1) {
    const _ok = _bind$4;
    _ok._0;
  } else {
    return _bind$4;
  }
  return _M0FP311moonbitlang4core7builtin10assert__eqGiE(game.state.log.length, 5, undefined, "@astrata/astrata/main:game_update_wbtest.mbt:70:3-70:40");
}
function _M0IP016_24default__implP37astrata7astrata4main28MoonBit__Async__Test__Driver17run__async__testsGRP37astrata7astrata4main34MoonBit__Async__Test__Driver__ImplE(_discard_) {}
function _M0IP016_24default__implP37astrata7astrata4main21MoonBit__Test__Driver9run__testGRP37astrata7astrata4main43MoonBit__Test__Driver__Internal__With__ArgsE(_discard_, _discard_$2, _discard_$3, _discard_$4, _discard_$5) {
  return new Result$Ok$3$(false);
}
function _M0IP016_24default__implP37astrata7astrata4main21MoonBit__Test__Driver9run__testGRP37astrata7astrata4main48MoonBit__Test__Driver__Internal__Async__No__ArgsE(_discard_, _discard_$2, _discard_$3, _discard_$4, _discard_$5) {
  return new Result$Ok$3$(false);
}
function _M0IP016_24default__implP37astrata7astrata4main21MoonBit__Test__Driver9run__testGRP37astrata7astrata4main50MoonBit__Test__Driver__Internal__Async__With__ArgsE(_discard_, _discard_$2, _discard_$3, _discard_$4, _discard_$5) {
  return new Result$Ok$3$(false);
}
function _M0IP016_24default__implP37astrata7astrata4main21MoonBit__Test__Driver9run__testGRP37astrata7astrata4main50MoonBit__Test__Driver__Internal__With__Bench__ArgsE(_discard_, _discard_$2, _discard_$3, _discard_$4, _discard_$5) {
  return new Result$Ok$3$(false);
}
function _M0FP37astrata7astrata4main45moonbit__test__driver__internal__catch__error(f, on_ok, on_err) {
  _M0FP37astrata7astrata4main42moonbit__test__driver__internal__js__catch(() => {
    let _try_err;
    _L: {
      const _bind$4 = f();
      if (_bind$4.$tag === 1) {
        const _ok = _bind$4;
        _ok._0;
      } else {
        const _err = _bind$4;
        _try_err = _err._0;
        break _L;
      }
      on_ok();
      return;
    }
    const err = _try_err;
    on_err(err);
  }, (err_str) => {
    on_err(new Error$astrata$47$astrata$47$main$46$MoonBitTestDriverInternalJsError$46$MoonBitTestDriverInternalJsError(err_str));
  });
}
function _M0IP37astrata7astrata4main41MoonBit__Test__Driver__Internal__No__ArgsP37astrata7astrata4main21MoonBit__Test__Driver9run__test(_discard_, filename, index, handle_result, error_to_string) {
  let index_map;
  _L: {
    const _bind$4 = _M0MP311moonbitlang4core7builtin3Map3getGsRP311moonbitlang4core7builtin3MapGiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEEE(_M0FP37astrata7astrata4main48moonbit__test__driver__internal__no__args__tests, filename);
    if (_bind$4 === undefined) {
      return new Result$Ok$3$(false);
    } else {
      const _Some = _bind$4;
      const _index_map = _Some;
      index_map = _index_map;
      break _L;
    }
  }
  let f;
  let attrs;
  _L$2: {
    const _bind$4 = _M0MP311moonbitlang4core7builtin3Map3getGiUWEuQRP311moonbitlang4core5error5ErrorRP311moonbitlang4core7builtin5ArrayGsEEE(index_map, index);
    if (_bind$4 === undefined) {
      return new Result$Ok$3$(false);
    } else {
      const _Some = _bind$4;
      const _x = _Some;
      const _f = _x._0;
      const _attrs = _x._1;
      f = _f;
      attrs = _attrs;
      break _L$2;
    }
  }
  let name;
  if (attrs.length >= 1) {
    const _name = attrs[0];
    name = _name;
  } else {
    name = "";
  }
  _M0FP37astrata7astrata4main45moonbit__test__driver__internal__catch__error(f, () => {
    handle_result(name, "", false);
  }, (err) => {
    handle_result(name, error_to_string(err), false);
  });
  return new Result$Ok$3$(true);
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
        case 5: {
          const _InspectError = err;
          const _e$2 = _InspectError._0;
          return _e$2;
        }
        case 6: {
          const _SnapshotError = err;
          const _e$3 = _SnapshotError._0;
          return _e$3;
        }
        case 4: {
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
    let _tmp$2;
    const _bind$4 = _M0IP37astrata7astrata4main41MoonBit__Test__Driver__Internal__No__ArgsP37astrata7astrata4main21MoonBit__Test__Driver9run__test(async_tests, filename, index, handle_result, error_to_string);
    let _tmp$3;
    if (_bind$4.$tag === 1) {
      const _ok = _bind$4;
      _tmp$3 = _ok._0;
    } else {
      const _err = _bind$4;
      _try_err = _err._0;
      break _L;
    }
    if (_tmp$3) {
      _tmp$2 = true;
    } else {
      let _tmp$4;
      const _bind$5 = _M0IP016_24default__implP37astrata7astrata4main21MoonBit__Test__Driver9run__testGRP37astrata7astrata4main43MoonBit__Test__Driver__Internal__With__ArgsE(async_tests, filename, index, handle_result, error_to_string);
      let _tmp$5;
      if (_bind$5.$tag === 1) {
        const _ok = _bind$5;
        _tmp$5 = _ok._0;
      } else {
        const _err = _bind$5;
        _try_err = _err._0;
        break _L;
      }
      if (_tmp$5) {
        _tmp$4 = true;
      } else {
        let _tmp$6;
        const _bind$6 = _M0IP016_24default__implP37astrata7astrata4main21MoonBit__Test__Driver9run__testGRP37astrata7astrata4main48MoonBit__Test__Driver__Internal__Async__No__ArgsE(async_tests, filename, index, handle_result, error_to_string);
        let _tmp$7;
        if (_bind$6.$tag === 1) {
          const _ok = _bind$6;
          _tmp$7 = _ok._0;
        } else {
          const _err = _bind$6;
          _try_err = _err._0;
          break _L;
        }
        if (_tmp$7) {
          _tmp$6 = true;
        } else {
          let _tmp$8;
          const _bind$7 = _M0IP016_24default__implP37astrata7astrata4main21MoonBit__Test__Driver9run__testGRP37astrata7astrata4main50MoonBit__Test__Driver__Internal__Async__With__ArgsE(async_tests, filename, index, handle_result, error_to_string);
          let _tmp$9;
          if (_bind$7.$tag === 1) {
            const _ok = _bind$7;
            _tmp$9 = _ok._0;
          } else {
            const _err = _bind$7;
            _try_err = _err._0;
            break _L;
          }
          if (_tmp$9) {
            _tmp$8 = true;
          } else {
            const _bind$8 = _M0IP016_24default__implP37astrata7astrata4main21MoonBit__Test__Driver9run__testGRP37astrata7astrata4main50MoonBit__Test__Driver__Internal__With__Bench__ArgsE(async_tests, filename, index, handle_result, error_to_string);
            let _tmp$10;
            if (_bind$8.$tag === 1) {
              const _ok = _bind$8;
              _tmp$10 = _ok._0;
            } else {
              const _err = _bind$8;
              _try_err = _err._0;
              break _L;
            }
            _tmp$8 = _tmp$10;
          }
          _tmp$6 = _tmp$8;
        }
        _tmp$4 = _tmp$6;
      }
      _tmp$2 = _tmp$4;
    }
    if (!_tmp$2) {
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
  let _tmp$2 = _start22;
  while (true) {
    const i = _tmp$2;
    if (i < _end23) {
      const entry = _M0MP311moonbitlang4core5array5Array2atGRP37astrata7astrata4main34MoonBitTestDriverInternalTestEntryE(tests, i);
      _M0FP37astrata7astrata4main44moonbit__test__driver__internal__do__execute(async_tests, _M0MP37astrata7astrata4main34MoonBitTestDriverInternalTestEntry8filename(entry), _M0MP37astrata7astrata4main34MoonBitTestDriverInternalTestEntry5index(entry));
      _tmp$2 = i + 1 | 0;
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
//# sourceMappingURL=main.whitebox_test.js.map
