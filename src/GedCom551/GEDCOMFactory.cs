using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Runtime.InteropServices;

namespace GedCom551
{
	// sample: TempFactory.Register("Temp1", new Func<ITemp>(() => new Temp1()));
	// url: http://www.gutgames.com/post/Factory-Pattern-using-Generics-in-C.aspx

	public delegate TResult TagConstructor<in T1, in T2, in T3, in T4, out TResult>(T1 arg1, T2 arg2, T3 arg3, T4 arg4);

	public sealed class GEDCOMFactory
	{
		private static GEDCOMFactory _Instance = null;
		private Dictionary<string, TagConstructor<TGEDCOMObject, TGEDCOMObject, string, string, TGEDCOMCustomTag>> _Constructors = new Dictionary<string, TagConstructor<TGEDCOMObject, TGEDCOMObject, string, string, TGEDCOMCustomTag>>();

		public static GEDCOMFactory GetInstance()
		{
			if (_Instance == null) _Instance = new GEDCOMFactory();
			return _Instance;
		}

		public void Register(string Key, TagConstructor<TGEDCOMObject, TGEDCOMObject, string, string, TGEDCOMCustomTag> Constructor)
		{
			if (_Constructors.ContainsKey(Key))
				_Constructors[Key] = Constructor;
			else
				_Constructors.Add(Key, Constructor);
		}

		public TGEDCOMCustomTag Create(TGEDCOMObject AOwner, TGEDCOMObject AParent, string ATagName, string AValue)
		{
			TagConstructor<TGEDCOMObject, TGEDCOMObject, string, string, TGEDCOMCustomTag> constrValue;

			if (_Constructors.TryGetValue(ATagName, out constrValue)) {
				return constrValue(AOwner, AParent, ATagName, AValue);
			} else {
				return null;
			}
		}
	}
}
