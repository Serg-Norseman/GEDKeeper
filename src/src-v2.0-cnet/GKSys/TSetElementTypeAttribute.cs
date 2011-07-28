using System;
using System.ComponentModel;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GKSys
{

	public class TSetElementTypeAttribute : Attribute
	{
		private Type FElementType;

		[Browsable(false)]
		public Type ElementType
		{
			get
			{
				return this.FElementType;
			}
		}
		public TSetElementTypeAttribute(Type AElementType)
		{
			this.FElementType = AElementType;
		}
	}
}
