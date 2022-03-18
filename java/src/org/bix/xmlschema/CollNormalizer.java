package org.bix.xmlschema;

import java.util.ArrayList;
import java.util.List;

public class CollNormalizer {
	
	@SuppressWarnings("rawtypes")
	public static List choiceFn(final List data, final List elements) {
		final List result = new ArrayList<>();
		result.add(true);
		int dIx = 0;
		int nIx = 0;
		while (true) {
			if (nIx >= elements.size() || dIx >= data.size()) {
				while (dIx < data.size()) {
					//TODO
					dIx++;
				}
				return result;
			}
			
		}
		return result;
	}
	@SuppressWarnings("rawtypes")
	public static List normalizeCollData(final List data, final List names) {
		final List result = new ArrayList<>();
		int dIx = 0;
		int nIx = 0;
		while (true) {
			if (nIx >= names.size() || dIx >= data.size()) {
				while (dIx < data.size()) {
					result.add(data.get(dIx));
					dIx++;
				}
				return result;
			}
			final Object o = names.get(nIx);
			nIx++;
			if (o instanceof List) {
				final List nn = (List) o;
				final List newList = new ArrayList<>();
				result.add(newList);
				for (int j = 0; j < nn.size(); j++) {
					final List d = (List) data.get(dIx);
					if (nn.contains(d.get(0))) {
						newList.add(d);
					} else {
						result.add(d);
					}
					dIx++;
					if (dIx >= data.size()) return result;
				}
			} else {
				result.add(data.get(dIx));
				dIx++;
			}
		}
	}
}
