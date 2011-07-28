using System;
using System.Runtime.InteropServices;

namespace GKSys
{
	public interface _TPersistentHelper
	{
		void Assign(MarshalByRefObject Source);
		MarshalByRefObject GetOwner();
		void AssignTo(MarshalByRefObject Dest);
	}

	[ComVisible(false)]
	public class TPersistentHelper : TObjectHelper, _TPersistentHelper
	{

		public static void AssignError(MarshalByRefObject Self, MarshalByRefObject Source)
		{
			/*string SourceName;
			if (Source != null)
			{
				SourceName = TObjectHelper.ClassName(BDSSystem.GetMetaFromObject(Source));
			}
			else
			{
				SourceName = "nil";
			}
			throw new EConvertError(string.Format("Cannot assign a %s to a %s", new object[]
			{
				SourceName, 
				TObjectHelper.ClassName(BDSSystem.GetMetaFromObject(Self))
			}));*/
			//alert!!!
		}

		public static void AssignTo(MarshalByRefObject Self, MarshalByRefObject Dest)
		{
			TPersistentHelper.AssignError(Dest, Self);
		}

		public static MarshalByRefObject GetOwner(MarshalByRefObject Self)
		{
			return null;
		}

		public static MarshalByRefObject Create(MarshalByRefObject Self)
		{
			return Self;
		}

		public static void Assign(MarshalByRefObject Self, MarshalByRefObject Source)
		{
			if (Source != null)
			{
				//(System.GetHelperIntf(Source, typeof(TPersistentHelper)) as _TPersistentHelper).AssignTo(Self);
				//alert!!! restore!
			}
			else
			{
				TPersistentHelper.AssignError(Self, null);
			}
		}

		void _TPersistentHelper.Assign(MarshalByRefObject Source)
		{
			TPersistentHelper.Assign(this.FInstance as MarshalByRefObject, Source);
		}

		MarshalByRefObject _TPersistentHelper.GetOwner()
		{
			return TPersistentHelper.GetOwner(this.FInstance as MarshalByRefObject);
		}

		void _TPersistentHelper.AssignTo(MarshalByRefObject Dest)
		{
			TPersistentHelper.AssignTo(this.FInstance as MarshalByRefObject, Dest);
		}

		public void AssignError(MarshalByRefObject Source)
		{
			TPersistentHelper.AssignError(this.FInstance as MarshalByRefObject, Source);
		}
	}
}
