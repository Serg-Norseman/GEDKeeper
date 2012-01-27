using System;
using System.IO;
using System.Runtime.InteropServices;

using GKSys;

namespace GedCom551
{
	public sealed class TGEDCOMGroupRecord : TGEDCOMRecord
	{
		private TGEDCOMListEx<TGEDCOMPointer> _Members;

		public TGEDCOMListEx<TGEDCOMPointer> Members
		{
			get { return this._Members; }
		}

		public string GroupName
		{
			get { return base.GetTagStringValue("NAME"); }
			set { base.SetTagStringValue("NAME", value); }
		}

		protected override void CreateObj(TGEDCOMTree AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			base.SetLists(EnumSet.Create(new Enum[]
			{
				TGEDCOMSubList.stNotes, TGEDCOMSubList.stMultimedia
			}));
			this.FRecordType = TGEDCOMRecordType.rtGroup;
			this.FName = "_GROUP";

			this._Members = new TGEDCOMListEx<TGEDCOMPointer>(this);
		}

		public override void Dispose()
		{
			if (!this.Disposed_)
			{
				this._Members.Dispose();

				base.Dispose();
				this.Disposed_ = true;
			}
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, TagConstructor ATagConstructor)
		{
			TGEDCOMTag Result;

			if (ATag == "NAME")
			{
				Result = base.AddTag(ATag, AValue, null);
			}
			else
			{
				if (ATag == "_MEMBER")
				{
					Result = this._Members.Add(new TGEDCOMPointer(base.Owner, this, ATag, AValue));
				}
				else
				{
					Result = base.AddTag(ATag, AValue, ATagConstructor);
				}
			}

			return Result;
		}

		public override void Clear()
		{
			base.Clear();
			this._Members.Clear();
		}

		public override bool IsEmpty()
		{
			return base.IsEmpty() && this._Members.Count == 0;
		}

		public override void ReplaceXRefs(TXRefReplaceMap aMap)
		{
			base.ReplaceXRefs(aMap);
			this._Members.ReplaceXRefs(aMap);
		}

		public override void ResetOwner(TGEDCOMTree AOwner)
		{
			base.ResetOwner(AOwner);
			this._Members.ResetOwner(AOwner);
		}

		public override void SaveToStream(StreamWriter AStream)
		{
			base.SaveToStream(AStream);
			this._Members.SaveToStream(AStream);
		}

		public int IndexOfMember(TGEDCOMIndividualRecord aMember)
		{
			int Result = -1;
			int num = this._Members.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				if (this._Members[i].XRef == aMember.XRef)
				{
					Result = i;
					break;
				}
			}
			return Result;
		}

		public TGEDCOMGroupRecord(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}

		public new static TGEDCOMCustomTag Create(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue)
		{
			return new TGEDCOMGroupRecord(AOwner, AParent, AName, AValue);
		}
	}
}
