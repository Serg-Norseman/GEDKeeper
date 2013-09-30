using System;
using System.IO;

using Ext.Utils;

/// <summary>
/// Localization: clean
/// </summary>

namespace GedCom551
{
	public sealed class TGEDCOMPersonalName : TGEDCOMTag
	{
		private TGEDCOMPersonalNamePieces _Pieces;

		public string FullName
		{
			get { return this.GetFullName(); }
		}

		public string FirstPart
		{
			get { return this.GetFirstPart(); }
		}

		public string Surname
		{
			get { return this.GetSurname(); }
			set { this.SetSurname(value); }
		}

		public string LastPart
		{
			get { return this.GetLastPart(); }
		}

		public TGEDCOMPersonalNamePieces Pieces
		{
			get { return this._Pieces; }
		}

		public TGEDCOMNameType NameType
		{
			get { return GEDCOMUtils.GetNameTypeVal(base.GetTagStringValue("TYPE").Trim().ToLower()); }
			set { base.SetTagStringValue("TYPE", GEDCOMUtils.GetNameTypeStr(value)); }
		}

		public void GetNameParts(out string firstPart, out string surname/*, out string ALastPart*/)
		{
			string sv = base.StringValue;
			if (sv == null || sv.Length == 0) {
				firstPart = "";
				surname = "";
			} else {
				int p = sv.IndexOf('/');

				if (p < 0) {
					firstPart = "";
				} else {
					firstPart = sv.Substring(0, p);
					firstPart = SysUtils.TrimRight(firstPart);
				}

				int p2 = ((p < 0) ? -1 : sv.IndexOf('/', p + 1));

				if (p < 0 || p2 < 0) {
					surname = "";
				} else {
					p++;
					surname = sv.Substring(p, p2 - p);
				}
			}

			//ALastPart = GetLastPart();
		}

		private string GetFirstPart()
		{
			string result;

			string sv = base.StringValue;
			if (sv == null || sv.Length == 0) {
				result = "";
			} else {
				int p = sv.IndexOf('/');
				if (p < 0) {
					result = "";
				} else {
					result = sv.Substring(0, p);
					result = SysUtils.TrimRight(result);
				}
			}

			return result;
		}

		private string GetSurname()
		{
			string result;

			string sv = base.StringValue;
			if (sv == null || sv.Length == 0) {
				result = "";
			} else {
				int p = sv.IndexOf('/');
				int p2 = ((p < 0) ? -1 : sv.IndexOf('/', p + 1));

				if (p < 0 || p2 < 0) {
					result = "";
				} else {
					p++;
					result = sv.Substring(p, p2 - p);
				}
			}

			return result;
		}

		private string GetLastPart()
		{
			string result = "";

			string sv = base.StringValue;
			int p = sv.IndexOf('/');
			if (p >= 0)
			{
				p = sv.IndexOf('/', p + 1);
				if (p >= 0)
				{
					result = SysUtils.TrimLeft(sv.Substring(p + 1));
				}
			}

			return result;
		}

		private string GetFullName()
		{
			string result = base.StringValue;
			while (result.IndexOf('/') >= 0)
			{
				result = result.Remove(result.IndexOf('/'), 1);
			}
			return result;
		}

		private void SetSurname(string Value)
		{
			base.StringValue = string.Concat(new string[]
			{
				SysUtils.TrimLeft(this.FirstPart + " "), "/", Value, "/", SysUtils.TrimRight(" " + this.LastPart)
			});
		}

		protected override void CreateObj(TGEDCOMTree owner, TGEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.FName = "NAME";

			this._Pieces = new TGEDCOMPersonalNamePieces(owner, this, "", "");
			this._Pieces.SetLevel(base.Level);
		}

		public override void Dispose()
		{
			if (!this.Disposed_)
			{
				this._Pieces.Dispose();

				base.Dispose();
				this.Disposed_ = true;
			}
		}

		public override TGEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			TGEDCOMTag result;

			if (tagName == "TYPE" || tagName == "FONE" || tagName == "ROMN")
			{
				result = base.AddTag(tagName, tagValue, tagConstructor);
			}
			else
			{
				result = this._Pieces.AddTag(tagName, tagValue, tagConstructor);
			}

			return result;
		}

        public override void Assign(TGEDCOMTag Source)
		{
			base.Assign(Source);

			if (Source is TGEDCOMPersonalName)
			{
				this._Pieces.Assign((Source as TGEDCOMPersonalName).Pieces);
			}
		}

		public override void Clear()
		{
			base.Clear();
			if (this._Pieces != null) this._Pieces.Clear();
		}

		public override bool IsEmpty()
		{
			return base.IsEmpty() && this._Pieces.IsEmpty();
		}

		public override void Pack()
		{
			base.Pack();
			this._Pieces.Pack();
		}

		public override void ReplaceXRefs(TXRefReplaceMap map)
		{
			base.ReplaceXRefs(map);
			this._Pieces.ReplaceXRefs(map);
		}

		public override void ResetOwner(TGEDCOMTree owner)
		{
			base.ResetOwner(owner);
			this._Pieces.ResetOwner(owner);
		}

		public override void SaveToStream(StreamWriter stream)
		{
			base.SaveToStream(stream);
			this._Pieces.SaveToStream(stream);
		}

		public void SetNameParts(string FirstPart, string Surname, string LastPart)
		{
			base.StringValue = SysUtils.TrimLeft(FirstPart + " ") + "/" + Surname + "/" + SysUtils.TrimRight(" " + LastPart);
		}

		public float IsMatch(TGEDCOMPersonalName name)
		{
			float match = 0F;
			int parts = 0;

			// FIXME: perform soundex check as well?
			// how would that effect returning a % match?
			float matches = 0;

			bool surnameMatched = false;

			if (!(string.IsNullOrEmpty(name.FirstPart) && string.IsNullOrEmpty(FirstPart)))
			{
				parts ++;
				if (name.FirstPart == FirstPart) matches ++;
			}

			if (!(string.IsNullOrEmpty(name.Surname) && string.IsNullOrEmpty(Surname)))
			{
				if ((name.Surname == "?" && Surname == "?") ||
					((string.Compare(name.Surname, "unknown", true) == 0) &&
					 (string.Compare(Surname, "unknown", true) == 0)))
				{
					// not really matched, surname isn't known,
					// don't count as part being checked, and don't penalize
					surnameMatched = true;
				}
				else
				{
					parts ++;
					if (name.Surname == Surname)
					{
						matches ++;
						surnameMatched = true;
					}
				}
			}
			else
			{
				// pretend the surname matches
				surnameMatched = true;
			}

			if (!(string.IsNullOrEmpty(name.Pieces.Prefix) && string.IsNullOrEmpty(Pieces.Prefix)))
			{
				parts ++;
				if (name.Pieces.Prefix == Pieces.Prefix) matches ++;
			}

			if (!(string.IsNullOrEmpty(name.Pieces.SurnamePrefix) && string.IsNullOrEmpty(Pieces.SurnamePrefix)))
			{
				parts ++;
				if (name.Pieces.SurnamePrefix == Pieces.SurnamePrefix) matches ++;
			}

			if (!(string.IsNullOrEmpty(name.Pieces.Suffix) && string.IsNullOrEmpty(Pieces.Suffix)))
			{
				parts ++;
				if (name.Pieces.Suffix == Pieces.Suffix) matches ++;
			}

			if (!(string.IsNullOrEmpty(name.Pieces.Nickname) && string.IsNullOrEmpty(Pieces.Nickname)))
			{
				parts ++;
				if (name.Pieces.Nickname == Pieces.Nickname) matches ++;
			}

			match = (matches / parts) * 100.0F;

			// FIXME: heavily penalise the surname not matching
			// for this to work correctly better matching needs to be
			// performed, not just string comparison
			if (!surnameMatched)
			{
				match *= 0.25F;
			}

			return match;
		}

		public TGEDCOMPersonalName(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMPersonalName(owner, parent, tagName, tagValue);
		}
	}
}
