using System;
using System.Collections;
using System.Globalization;
using System.Reflection;
using System.Runtime.InteropServices;

namespace GKSys
{
	public class TClassHelperBase
	{
		public object FInstance;
	}

	[ComVisible(false)]
	public class TObjectHelper : TClassHelperBase
	{

		public static void Free(object Self)
		{
			if (Self != null && Self is IDisposable)
			{
				IFreeNotify freeNotify = Self as IFreeNotify;
				if (freeNotify != null)
				{
					freeNotify.BeforeFree();
				}
				((IDisposable)Self).Dispose();
			}
		}

		public void Free()
		{
			TObjectHelper.Free(this.FInstance);
		}
	}
}
