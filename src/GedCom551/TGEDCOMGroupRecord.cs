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

		public override void ReplaceXRefs(XRefReplacer replacer)
		{
			base.ReplaceXRefs(replacer);
			this.fMembers.ReplaceXRefs(replacer);
		}

		public override void ResetOwner(TGEDCOMTree owner)
		{
			base.ResetOwner(owner);
			this.fMembers.ResetOwner(owner);
		}

		public override void SaveToStream(StreamWriter stream)
		{
			base.SaveToStream(stream);
			this.fMembers.SaveToStream(stream);
		}

		public int IndexOfMember(TGEDCOMIndividualRecord member)
		{
			int Result = -1;
			int num = this.fMembers.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				if (this.fMembers[i].XRef == member.XRef)
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

        public bool aux_AddMember(TGEDCOMIndividualRecord member)
		{
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
			catch (Exception E)
			{
				SysUtils.LogWrite("GKUtils.AddGroupMember(): " + E.Message);
				result = false;
			}

			return result;
		}

		public bool aux_RemoveMember(TGEDCOMIndividualRecord member)
		{
			bool result;

			try
			{
				this.Members.Delete(this.IndexOfMember(member));
				member.Groups.Delete(member.IndexOfGroup(this));

				result = true;
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKUtils.RemoveGroupMember(): " + E.Message);
				result = false;
			}

			return result;
		}

		#endregion
	}
}
