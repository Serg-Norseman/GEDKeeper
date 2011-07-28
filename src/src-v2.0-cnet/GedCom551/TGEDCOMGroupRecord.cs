using GKSys;
using System;
using System.ComponentModel;
using System.IO;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMGroupRecord : TGEDCOMRecord
	{
		internal TGEDCOMList FMembers;

		/*public TGEDCOMPointer Members
		{
			get { return this.GetMember(Index); }
		}
		public int MembersCount
		{
			get { return this.GetMembersCount(); }
		}*/

		public new string Name
		{
			get { return this.GetName(); }
			set { this.SetName(value); }
		}

		internal string GetName()
		{
			return base.GetTagStringValue("NAME");
		}

		internal void SetName([In] string Value)
		{
			base.SetTagStringValue("NAME", Value);
		}

		internal TGEDCOMPointer GetMember(int Index)
		{
			TGEDCOMPointer Result;
			if (this.FMembers == null || Index < 0 || Index >= this.FMembers.Count)
			{
				Result = null;
			}
			else
			{
				Result = (this.FMembers[Index] as TGEDCOMPointer);
			}
			return Result;
		}

		internal int GetMembersCount()
		{
			int Result;
			if (this.FMembers == null)
			{
				Result = 0;
			}
			else
			{
				Result = this.FMembers.Count;
			}
			return Result;
		}

		protected internal override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			base.SetLists(TEnumSet.Create(new Enum[]
			{
				TGEDCOMObject.TGEDCOMSubList.stNotes, 
				TGEDCOMObject.TGEDCOMSubList.stMultimedia
			}));
			this.FRecordType = TGEDCOMRecord.TGEDCOMRecordType.rtGroup;
			this.FName = "_GROUP";
			this.FMembers = null;
		}

		public override void Dispose()
		{
			if (!this.Disposed_)
			{
				if (this.FMembers != null)
				{
					object fMembers = this.FMembers;
					VCLUtils.FreeAndNil(ref fMembers);
					this.FMembers = (fMembers as TGEDCOMList);
				}
				base.Dispose();
				this.Disposed_ = true;
			}
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result;
			if (BDSSystem.WStrCmp(ATag, "NAME") == 0)
			{
				Result = base.AddTag(ATag, AValue, null);
			}
			else
			{
				if (BDSSystem.WStrCmp(ATag, "_MEMBER") == 0)
				{
					Result = this.AddMember(new TGEDCOMPointer(base.Owner, this, ATag, AValue));
				}
				else
				{
					Result = base.AddTag(ATag, AValue, AClass);
				}
			}
			return Result;
		}

		public override void Clear()
		{
			base.Clear();
			if (this.FMembers != null)
			{
				this.FMembers.Clear();
			}
		}

		public override bool IsEmpty()
		{
			return base.IsEmpty() && this.GetMembersCount() == 0;
		}

		public override void ReplaceXRefs(TXRefReplaceMap aMap)
		{
			base.ReplaceXRefs(aMap);
			if (this.FMembers != null)
			{
				this.FMembers.ReplaceXRefs(aMap);
			}
		}

		public override void ResetOwner(TGEDCOMObject AOwner)
		{
			base.ResetOwner(AOwner);
			if (this.FMembers != null)
			{
				this.FMembers.ResetOwner(AOwner);
			}
		}

		public override void SaveToStream(StreamWriter AStream)
		{
			base.SaveToStream(AStream);
			if (this.FMembers != null)
			{
				this.FMembers.SaveToStream(AStream);
			}
		}

		public TGEDCOMPointer AddMember(TGEDCOMPointer Value)
		{
			if (this.FMembers == null)
			{
				this.FMembers = new TGEDCOMList(this);
			}
			if (this.FMembers != null)
			{
				this.FMembers.Add(Value);
			}
			return Value;
		}

		public int IndexOfMember(TGEDCOMIndividualRecord aMember)
		{
			int Result = -1;
			int arg_11_0 = 0;
			int num = this.FMembers.Count - 1;
			int i = arg_11_0;
			if (num >= i)
			{
				num++;
				while (BDSSystem.WStrCmp((this.FMembers[i] as TGEDCOMPointer).XRef, aMember.XRef) != 0)
				{
					i++;
					if (i == num)
					{
						return Result;
					}
				}
				Result = i;
			}
			return Result;
		}

		public void DeleteMember(int aIndex)
		{
			if (this.FMembers != null)
			{
				this.FMembers.Delete(aIndex);
			}
		}

		public TGEDCOMGroupRecord(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
