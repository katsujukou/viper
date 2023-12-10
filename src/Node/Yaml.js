import { parse } from 'yaml';

export const parseImpl = (Left, Right, doc) => {
  try {
    const yaml = parse(doc);
    return Right(yaml);
  } catch (e) {
    return Left(e);
  }
}