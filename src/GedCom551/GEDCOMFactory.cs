using System;
using System.Collections.Generic;

namespace GedCom551
{
    public delegate TGEDCOMTag TagConstructor(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue);

	public sealed class GEDCOMFactory
	{
		private static GEDCOMFactory _Instance = null;
		private Dictionary<string, TagConstructor> _Constructors = new Dictionary<string, TagConstructor>();

		public static GEDCOMFactory GetInstance()
		{
			if (_Instance == null) _Instance = new GEDCOMFactory();
			return _Instance;
		}

		public void RegisterTag(string key, TagConstructor constructor)
		{
			if (_Constructors.ContainsKey(key))
				_Constructors[key] = constructor;
			else
				_Constructors.Add(key, constructor);
		}

        public TGEDCOMTag CreateTag(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue)
		{
			TagConstructor constructor;

			if (_Constructors.TryGetValue(tagName, out constructor)) {
				return constructor(owner, parent, tagName, tagValue);
			} else {
				return null;
			}
		}
	}
}
