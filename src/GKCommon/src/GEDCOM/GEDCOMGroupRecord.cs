using System;
using System.IO;
using ExtUtils;
using GKCommon.GEDCOM.Enums;

namespace GKCommon.GEDCOM
{
	public sealed class GEDCOMGroupRecord : GEDCOMRecord
	{
		private GEDCOMList<GEDCOMPointer> fMembers;

		public GEDCOMList<GEDCOMPointer> Members
		{
			get { return this.fMembers; }
		}

		public string GroupName
		{
			get { return base.GetTagStringValue("NAME"); }
			set { base.SetTagStringValue("NAME", value); }
		}

		protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fRecordType = GEDCOMRecordType.rtGroup;
			this.fName = "_GROUP";

			this.fMembers = new GEDCOMList<GEDCOMPointer>(this);
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing)
			{
				this.fMembers.Dispose();
			}
			base.Dispose(disposing);
		}

		public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			GEDCOMTag result;

			if (tagName == "NAME")
			{
				result = base.AddTag(tagName, tagValue, null);
			}
			else if (tagName == "_MEMBER")
			{
				result = this.fMembers.Add(new GEDCOMPointer(base.Owner, this, tagName, tagValue));
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

		public override void ResetOwner(GEDCOMTree newOwner)
		{
			base.ResetOwner(newOwner);
			this.fMembers.ResetOwner(newOwner);
		}

		public override void SaveToStream(StreamWriter stream)
		{
			base.SaveToStream(stream);
			this.fMembers.SaveToStream(stream);
		}

		public GEDCOMGroupRecord(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new GEDCOMGroupRecord(owner, parent, tagName, tagValue);
		}

        #region Auxiliary

		public int IndexOfMember(GEDCOMIndividualRecord member)
		{
			if (member != null) {
				int num = this.fMembers.Count;
				for (int i = 0; i < num; i++)
				{
					if (this.fMembers[i].XRef == member.XRef) {
						return i;
					}
				}
			}

			return -1;
		}

        public bool AddMember(GEDCOMIndividualRecord member)
        {
            if (member == null) return false;
			bool result;

			try
			{
				GEDCOMPointer ptr = new GEDCOMPointer(this.Owner, this, "", "");
				ptr.SetNamedValue("_MEMBER", member);
				this.Members.Add(ptr);

				ptr = new GEDCOMPointer(this.Owner, member, "", "");
				ptr.SetNamedValue("_GROUP", this);
				member.Groups.Add(ptr);

				result = true;
			}
			catch (Exception ex)
			{
				SysUtils.LogWrite("GEDCOMGroupRecord.AddMember(): " + ex.Message);
				result = false;
			}

			return result;
		}

		public bool RemoveMember(GEDCOMIndividualRecord member)
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
				SysUtils.LogWrite("GEDCOMGroupRecord.RemoveMember(): " + ex.Message);
				result = false;
			}

			return result;
		}

		#endregion
	}
}
