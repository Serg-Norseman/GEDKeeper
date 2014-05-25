using System;
using System.IO;

using ExtUtils;

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

		protected override void CreateObj(TGEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fRecordType = TGEDCOMRecordType.rtGroup;
			this.fName = "_GROUP";

			this.fMembers = new GEDCOMList<TGEDCOMPointer>(this);
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing)
			{
				this.fMembers.Dispose();
			}
			base.Dispose(disposing);
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

        public override void ReplaceXRefs(XRefReplacer map)
		{
            base.ReplaceXRefs(map);
            this.fMembers.ReplaceXRefs(map);
		}

		public override void ResetOwner(TGEDCOMTree newOwner)
		{
			base.ResetOwner(newOwner);
			this.fMembers.ResetOwner(newOwner);
		}

		public override void SaveToStream(StreamWriter stream)
		{
			base.SaveToStream(stream);
			this.fMembers.SaveToStream(stream);
		}

		public int IndexOfMember(TGEDCOMIndividualRecord member)
		{
			int result = -1;
            if (member == null) return result;

			int num = this.fMembers.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				if (this.fMembers[i].XRef == member.XRef)
				{
					result = i;
					break;
				}
			}
			return result;
		}

		public TGEDCOMGroupRecord(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMGroupRecord(owner, parent, tagName, tagValue);
		}

        #region Auxiliary

        public bool aux_AddMember(TGEDCOMIndividualRecord member)
        {
            if (member == null) return false;
			bool result;

			try
			{
				TGEDCOMPointer ptr = new TGEDCOMPointer(this.Owner, this, "", "");
				ptr.SetNamedValue("_MEMBER", member);
				this.Members.Add(ptr);

				ptr = new TGEDCOMPointer(this.Owner, member, "", "");
				ptr.SetNamedValue("_GROUP", this);
				member.Groups.Add(ptr);

				result = true;
			}
			catch (Exception ex)
			{
				SysUtils.LogWrite("TGEDCOMGroupRecord.AddGroupMember(): " + ex.Message);
				result = false;
			}

			return result;
		}

		public bool aux_RemoveMember(TGEDCOMIndividualRecord member)
		{
            if (member == null) return false;
			bool result;

			try
			{
				this.Members.Delete(this.IndexOfMember(member));
				member.Groups.Delete(member.IndexOfGroup(this));

				result = true;
			}
			catch (Exception ex)
			{
				SysUtils.LogWrite("TGEDCOMGroupRecord.RemoveGroupMember(): " + ex.Message);
				result = false;
			}

			return result;
		}

		#endregion
	}
}
