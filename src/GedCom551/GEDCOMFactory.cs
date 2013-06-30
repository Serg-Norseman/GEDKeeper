using System;
using System.Collections.Generic;

namespace GedCom551
{
    public delegate TGEDCOMTag TagConstructor(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue);

	public sealed class GEDCOMFactory
	{
		private static GEDCOMFactory fInstance = null;
		private Dictionary<string, TagConstructor> fConstructors = new Dictionary<string, TagConstructor>();

		public static GEDCOMFactory GetInstance()
		{
			if (fInstance == null) fInstance = new GEDCOMFactory();
			return fInstance;
		}

		public void RegisterTag(string key, TagConstructor constructor)
		{
			if (fConstructors.ContainsKey(key))
				fConstructors[key] = constructor;
			else
				fConstructors.Add(key, constructor);
		}

        public TGEDCOMTag CreateTag(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue)
		{
			TagConstructor constructor;

			if (fConstructors.TryGetValue(tagName, out constructor)) {
				return constructor(owner, parent, tagName, tagValue);
			} else {
				return null;
			}
		}
	}
}
