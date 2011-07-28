using GKSys;
using System;
using System.Runtime.CompilerServices;

namespace GKSys
{
	public class TClass
	{
		protected RuntimeTypeHandle FInstanceTypeHandle;
		protected Type FInstanceType;
		protected TClass FClassParent;
		protected internal void SetInstanceType(RuntimeTypeHandle ATypeHandle)
		{
			if (!this.FInstanceTypeHandle.Equals(ATypeHandle))
			{
				this.FInstanceTypeHandle = ATypeHandle;
				this.FClassParent = null;
			}
		}
		protected internal void SetDelegator(Type ATypeDelegator)
		{
			this.FClassParent = null;
			this.FInstanceType = ATypeDelegator;
		}
		public TClass()
		{
		}
		public TClass(RuntimeTypeHandle ATypeHandle)
		{
			this.FInstanceTypeHandle = ATypeHandle;
		}
		public TClass(Type AType) : this(AType.TypeHandle)
		{
		}

		public RuntimeTypeHandle InstanceTypeHandle()
		{
			RuntimeTypeHandle result;
			if (this.FInstanceType != null)
			{
				result = this.FInstanceType.TypeHandle;
			}
			else
			{
				result = this.FInstanceTypeHandle;
			}
			return result;
		}
		public Type InstanceType()
		{
			Type result;
			if (this.FInstanceType != null)
			{
				result = this.FInstanceType;
			}
			else
			{
				result = Type.GetTypeFromHandle(this.FInstanceTypeHandle);
			}
			return result;
		}
		public override bool Equals(object AObj)
		{
			bool result = false;
			if (AObj != null)
			{
				TClass TClass = AObj as TClass;
				if (TClass != null && this.FInstanceTypeHandle.Equals(TClass.FInstanceTypeHandle))
				{
					result = true;
				}
			}
			return result;
		}

		public override int GetHashCode()
		{
			return this.FInstanceTypeHandle.GetHashCode();
		}
	}
}
