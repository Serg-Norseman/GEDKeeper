using System;
using System.Collections.Generic;

namespace GedCom551
{
	// sample: TempFactory.Register("Temp1", new Func<ITemp>(() => new Temp1()));
	// url: http://www.gutgames.com/post/Factory-Pattern-using-Generics-in-C.aspx

	public delegate TGEDCOMCustomTag TagConstructor(TGEDCOMTree AOwner, TGEDCOMObject AParent, string ATagName, string AValue);

	public sealed class GEDCOMFactory
	{
		private static GEDCOMFactory _Instance = null;
		private Dictionary<string, TagConstructor> _Constructors = new Dictionary<string, TagConstructor>();

		public static GEDCOMFactory GetInstance()
		{
			if (_Instance == null) _Instance = new GEDCOMFactory();
			return _Instance;
		}

		public void Register(string Key, TagConstructor constructor)
		{
			if (_Constructors.ContainsKey(Key))
				_Constructors[Key] = constructor;
			else
				_Constructors.Add(Key, constructor);
		}

		public TGEDCOMCustomTag Create(TGEDCOMTree AOwner, TGEDCOMObject AParent, string ATagName, string AValue)
		{
			TagConstructor constructor;

			if (_Constructors.TryGetValue(ATagName, out constructor)) {
				return constructor(AOwner, AParent, ATagName, AValue);
			} else {
				return null;
			}
		}
	}
}
