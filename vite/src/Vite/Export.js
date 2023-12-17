export const jsNull = null;

export const jsUndefined = void 0;

export const isNull = function(v) {
  return v === null;
}

export const isEmptyRecord = function (obj) {
  return Object.keys(obj).length == 0;
}