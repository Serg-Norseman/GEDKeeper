using System;
using System.ComponentModel;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GKSys
{
	public class TUInt32SubrangeAttribute : Attribute
	{
		private uint FMinValue;
		private uint FMaxValue;
		private Type FBaseType;
		[Browsable(false)]
		public uint MinValue
		{
			get
			{
				return this.FMinValue;
			}
		}
		[Browsable(false)]
		public uint MaxValue
		{
			get
			{
				return this.FMaxValue;
			}
		}
		[Browsable(false)]
		public Type BaseType
		{
			get
			{
				return this.FBaseType;
			}
		}
		public TUInt32SubrangeAttribute(uint AMinValue, uint AMaxValue, Type ABaseType)
		{
			this.FMinValue = AMinValue;
			this.FMaxValue = AMaxValue;
			this.FBaseType = ABaseType;
		}
	}
}
