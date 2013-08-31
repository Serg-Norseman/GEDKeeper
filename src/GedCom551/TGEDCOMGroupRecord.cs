using System;
using System.IO;

using Ext.Utils;

namespace GedCom551
{
	public sealed class TGEDCOMGroupRecord : TGEDCOMRecord
	{
		private GEDCOMList<TGEDCOMPointer> fMembers;

		public GEDCOMList<TGEDCOMPointer> Members
		{
			get { return this.fMembers; }
		}

		public string GroupName
		{
			get { return base.GetTagStringValue("NAME"); }
			set { base.SetTagStringValue("NAME", value); }
		}

		protected override void CreateObj(TGEDCOMTree owner, TGEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.FRecordType = TGEDCOMRecordType.rtGroup;
			this.FName = "_GROUP";

			this.fMembers = new GEDCOMList<TGEDCOMPointer>(this);
		}

		public override void Dispose()
		{
			if (!this.Disposed_)
			{
				this.fMembers.Dispose();

				base.Dispose();
				this.Disposed_ = true;
			}
		}

		public override TGEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			TGEDCOMTag result;

			if (tagName == "NAME")
			{
				result = base.AddTag(tagName, tagValue, null);
			}
			else if (tagName == "_MEMBER")
			{
				result = this.fMembers.Add(new TGEDCOMPointer(base.Owner, this, tagName, tagValue));
			}
			else
			{
				result = base.AddTag(tagName, tagValue, tagConstructor);
			}

			return result;
		}

		public override void Clear()
		{
			base.Clear();
			this.fMembers.Clear();
		}

		public override bool IsEmpty()
		{
			return base.IsEmpty() && this.fMembers.Count == 0;
		}

		public override void ReplaceXRefs(TXRefReplaceMap aMap)
		{
			base.ReplaceXRefs(aMap);
			this.fMembers.ReplaceXRefs(aMap);
		}

		public override void ResetOwner(TGEDCOMTree AOwner)
		{
			base.ResetOwner(AOwner);
			this.fMembers.ResetOwner(AOwner);
		}

		public override void SaveToStream(StreamWriter AStream)
		{
			base.SaveToStream(AStream);
			this.fMembers.SaveToStream(AStream);
		}

		public int IndexOfMember(TGEDCOMIndividualRecord aMember)
		{
			int Result = -1;
			int num = this.fMembers.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				if (this.fMembers[i].XRef == aMember.XRef)
				{
					Result = i;
					break;
				}
			}
			return Result;
		}

		public TGEDCOMGroupRecord(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMGroupRecord(owner, parent, tagName, tagValue);
		}

        #region Auxiliary

        public bool aux_AddMember(TGEDCOMIndividualRecord aMember)
		{
			bool Result;
			try
			{
				TGEDCOMPointer ptr = new TGEDCOMPointer(this.Owner, this, "", "");
				ptr.SetNamedValue("_MEMBER", aMember);
				this.Members.Add(ptr);
				ptr = new TGEDCOMPointer(this.Owner, aMember, "", "");
				ptr.SetNamedValue("_GROUP", this);
				aMember.Groups.Add(ptr);
				Result = true;
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("TGenEngine.AddGroupMember(): " + E.Message);
				Result = false;
			}
			return Result;
		}

		public bool aux_RemoveMember(TGEDCOMIndividualRecord aMember)
		{
			bool Result;
			try
			{
				this.Members.Delete(this.IndexOfMember(aMember));
				aMember.Groups.Delete(aMember.IndexOfGroup(this));
				Result = true;
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("TGenEngine.RemoveGroupMember(): " + E.Message);
				Result = false;
			}
			return Result;
		}

		#endregion
	}
}
