using System;
using System.Globalization;
using System.IO;

namespace GKCommon.GEDCOM
{
	public class GEDCOMTag : GEDCOMObject
	{
		#region Protected fields

		protected int fLevel;
		protected GEDCOMTree fOwner;
		protected string fName;
		protected GEDCOMObject fParent;
		protected string fStringValue;
		protected GEDCOMList<GEDCOMTag> fTags;

		#endregion
		
		#region Public properties
		
		public int Count
		{
			get { return this.fTags.Count; }
		}

		public GEDCOMTag this[int index]
		{
			get { return this.fTags[index]; }
		}

		public int Level
		{
			get { return this.fLevel; }
		}

		public string Name
		{
			get { return this.fName; }
			set { this.fName = value; }
		}

		public GEDCOMTree Owner
		{
			get { return this.fOwner; }
		}

		public GEDCOMObject Parent
		{
			get { return this.fParent; }
		}

		public string StringValue
		{
			get { return this.GetStringValue(); }
			set { this.SetStringValue(value); }
		}

		#endregion
		
		#region Object management
		
		protected virtual void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
		{
			this.fOwner = owner;
			this.fParent = parent;
			this.fTags = new GEDCOMList<GEDCOMTag>(this);
			this.fStringValue = "";

			if (parent != null && parent is GEDCOMTag) {
				this.fLevel = (parent as GEDCOMTag).Level + 1;
			} else {
				this.fLevel = 0;
			}
		}

		public GEDCOMTag(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			this.CreateObj(owner, parent);

			if (tagName != "" || tagValue != "")
			{
				this.Name = tagName;
				this.SetStringValue(tagValue);
			}
		}

        protected override void Dispose(bool disposing)
		{
			if (disposing)
			{
				if (this.fTags != null) {
					this.fTags.Dispose();
					this.fTags = null;
				}
			}
            base.Dispose(disposing);
		}

		public static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
            return new GEDCOMTag(owner, parent, tagName, tagValue);
		}

		#endregion
		
        #region Content management

		protected GEDCOMRecord FindRecord(string xref)
		{
			return ((this.fOwner == null) ? null : this.fOwner.XRefIndex_Find(xref));
		}

		protected GEDCOMTag InsertTag(GEDCOMTag tag)
		{
			this.fTags.Add(tag);
			return tag;
		}

		public bool IsEmptySkip()
		{
			return GEDCOMUtils.GetTagProps(this.fName).EmptySkip;
		}

		public void SetLevel(int value)
		{
			this.fLevel = value;
		}

		public virtual GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			GEDCOMTag tag = null;
			try
			{
				if (tagConstructor != null) {
					tag = tagConstructor(this.fOwner, this, tagName, tagValue);
				} else {
					tag = GEDCOMFactory.GetInstance().CreateTag(this.fOwner, this, tagName, tagValue);
					if (tag == null) {
						tag = new GEDCOMTag(this.fOwner, this, tagName, tagValue);
					}
				}

				this.InsertTag(tag);
			}
			catch (Exception ex)
			{
				SysUtils.LogWrite("GEDCOMTag.InternalCreateTag(): " + ex.Message);
			}
			return tag;
		}

		public virtual void Assign(GEDCOMTag source)
		{
			if (source == null) return;
			
			this.Name = source.Name;
			this.StringValue = source.StringValue;

			int num = source.Count;
			for (int i = 0; i < num; i++)
			{
				GEDCOMTag sourceTag = source[i];
				GEDCOMTag copy = Activator.CreateInstance(sourceTag.GetType(), new object[] { this.Owner, this, "", "" }) as GEDCOMTag;
				copy.Assign(sourceTag);
				this.InsertTag(copy);
			}
		}

		protected void AssignList(GEDCOMList<GEDCOMTag> srcList, GEDCOMList<GEDCOMTag> destList)
		{
			int num = srcList.Count;
			for (int i = 0; i < num; i++)
			{
				GEDCOMTag sourceTag = srcList[i];
				GEDCOMTag copy = Activator.CreateInstance(sourceTag.GetType(), new object[] { this.Owner, this, "", "" }) as GEDCOMTag;
				copy.Assign(sourceTag);
				destList.Add(copy);
			}
		}

		public virtual void Clear()
		{
			this.fTags.Clear();
			this.fStringValue = "";
		}

		public void Delete(int index)
		{
			this.fTags.Delete(index);
		}

		public void DeleteTag(string tagName)
		{
			GEDCOMTag tag = this.FindTag(tagName, 0);
			if (tag != null) {
				do
				{
					int idx = this.fTags.IndexOfObject(tag);
					this.fTags.Delete(idx);
					tag = this.FindTag(tagName, idx);
				}
				while (tag != null);
			}
		}

		public GEDCOMTag FindTag(string tagName, int startIndex)
		{
			string SU = tagName.ToUpperInvariant();

			int pos = SU.IndexOf('\\');
			string S = ((pos >= 0) ? SU.Substring(0, pos) : SU);

			GEDCOMTag tempTag = this;
			GEDCOMTag resultTag;

			while (true)
			{
				int index = ((S == SU) ? startIndex : 0);

				while (index < tempTag.Count && tempTag[index].Name != S) index++;

				if (index >= tempTag.Count) break;

				resultTag = tempTag[index];
				tempTag = resultTag;

				pos = SU.IndexOf('\\');
				if (pos >= 0)
				{
					SU = SU.Substring(pos + 1);

					pos = SU.IndexOf('\\');
					S = ((pos >= 0) ? SU.Substring(0, pos) : SU);
				}
				else
				{
					SU = "";
				}

				if (SU == "") return resultTag;
			}

			resultTag = null;
			return resultTag;
		}

		public GEDCOMTag TagClass(string tagName, TagConstructor tagConstructor)
		{
			GEDCOMTag result = this.FindTag(tagName, 0);

			if (result == null) {
				result = this.AddTag(tagName, "", tagConstructor);
			}

			return result;
		}

		public int IndexOfTag(GEDCOMTag tag)
		{
			return this.fTags.IndexOfObject(tag);
		}

		public virtual bool IsEmpty()
		{
			return ((this.fStringValue == "") && (this.fTags.Count == 0));
		}
		
		public virtual float IsMatch(GEDCOMTag tag, MatchParams matchParams)
		{
			return 0.0f;
		}
		
		#endregion

		#region Values management

		protected virtual string GetStringValue()
		{
			return this.fStringValue;
		}

		protected virtual void SetStringValue(string S)
		{
			this.ParseString(S);
		}

		public virtual string ParseString(string strValue)
		{
			this.fStringValue = strValue;
			return string.Empty;
		}


		public int GetTagIntegerValue(string tagName, int defValue)
		{
			string S = this.GetTagStringValue(tagName);
			int result = ((S == "") ? defValue : SysUtils.ParseInt(S, defValue));
			return result;
		}

		public void SetTagIntegerValue(string tagName, int value)
		{
			this.SetTagStringValue(tagName, value.ToString());
		}


		public double GetTagFloatValue(string tagName, double defValue)
		{
			string S = this.GetTagStringValue(tagName);
			double result = ((S == "") ? defValue : SysUtils.ParseFloat(S, defValue));
			return result;
		}

		public void SetTagFloatValue(string tagName, double value)
		{
			NumberFormatInfo nfi = new NumberFormatInfo();
			nfi.NumberDecimalSeparator = ".";
			this.SetTagStringValue(tagName, value.ToString(nfi));
		}


		public string GetTagStringValue(string tagName)
		{
			GEDCOMTag tag = this.FindTag(tagName, 0);
			string result = ((tag == null) ? "" : tag.StringValue);
			return result;
		}

		public void SetTagStringValue(string tagName, string value)
		{
			string SU = tagName;

			GEDCOMTag P = this.FindTag(SU, 0);

			if (P != null)
			{
				P.StringValue = value;
			}
			else
			{
				GEDCOMTag O = this;
				while (SU != "")
				{
					string S;

					int Index = SU.IndexOf('\\');
					if (Index >= 0)
					{
						S = SU.Substring(0, Index);
						SU = SU.Substring(Index + 1);
					}
					else
					{
						S = SU;
						SU = "";
					}

					P = O.FindTag(S, 0);
					if (P == null)
					{
						if (SU == "")
						{
							P = O.AddTag(S, value, null);
						}
						else
						{
							P = O.AddTag(S, "", null);
						}
					}
					else
					{
						if (SU == "")
						{
							P.StringValue = value;
						}
					}
					O = P;
				}
			}
		}


		public StringList GetTagStrings(GEDCOMTag ATag)
		{
			StringList strings = new StringList();

			if (ATag != null)
			{
				if (ATag.StringValue != "") {
					strings.Add(ATag.StringValue);
				}

				int num = ATag.Count;
				for (int i = 0; i < num; i++)
				{
					GEDCOMTag tag = ATag[i];

					if (tag.Name == "CONC") {
						strings[strings.Count - 1] = strings[strings.Count - 1] + tag.StringValue;
					} else {
						if (tag.Name == "CONT") {
							strings.Add(tag.StringValue);
						}
					}
				}
			}

			return strings;
		}

		public void SetTagStrings(GEDCOMTag tag, StringList value)
		{
			if (tag != null)
			{
				tag.StringValue = "";
				for (int i = tag.Count - 1; i >= 0; i--)
				{
					if (tag[i].Name == "CONT" || tag[i].Name == "CONC")
					{
						tag.Delete(i);
					}
				}

				if (value != null)
				{
					int num = value.Count;
					for (int i = 0; i < num; i++)
					{
						string S = value[i];

						int len = ((S.Length > 248) ? 248 : S.Length) /*248*/;
						string sub = S.Substring(0, len);
						S = S.Remove(0, len);

						if (i == 0 && !(tag is GEDCOMRecord)) {
							tag.StringValue = sub;
						} else {
							tag.AddTag("CONT", sub, null);
						}

						while (((S != null) ? S.Length : 0) > 0)
						{
							len = ((S.Length > 248) ? 248 : S.Length) /*248*/;
							tag.AddTag("CONC", S.Substring(0, len), null);
							S = S.Remove(0, len);
						}
					}
				}
			}
		}

        public void SetTagStrings(GEDCOMTag tag, string[] strings)
		{
        	if (tag == null || strings == null) return;
        	
        	tag.StringValue = "";
        	for (int i = tag.Count - 1; i >= 0; i--)
        	{
        		if (tag[i].Name == "CONT" || tag[i].Name == "CONC")
        		{
        			tag.Delete(i);
        		}
        	}

        	for (int i = 0; i < strings.Length; i++)
        	{
        		string S = strings[i];

        		int len = ((S.Length > 248) ? 248 : S.Length) /*248*/;
        		string sub = S.Substring(0, len);
        		S = S.Remove(0, len);

        		if (i == 0 && !(tag is GEDCOMRecord))
        		{
        			tag.StringValue = sub;
        		}
        		else
        		{
        			tag.AddTag("CONT", sub, null);
        		}

        		while (((S != null) ? S.Length : 0) > 0)
        		{
        			len = ((S.Length > 248) ? 248 : S.Length) /*248*/;
        			tag.AddTag("CONC", S.Substring(0, len), null);
        			S = S.Remove(0, len);
        		}
        	}
        }

        #endregion

		#region Tree management

		public virtual void Pack()
		{
			this.fTags.Pack();
		}

		public virtual void ReplaceXRefs(XRefReplacer map)
		{
			this.fTags.ReplaceXRefs(map);
		}

		public virtual void ResetOwner(GEDCOMTree newOwner)
		{
			this.fOwner = newOwner;
			this.fTags.ResetOwner(newOwner);
		}

		public void ResetParent(GEDCOMObject parent)
		{
			this.fParent = parent;
		}

		#endregion
		
		#region Stream management

		protected virtual void SaveTagsToStream(StreamWriter stream)
		{
			if (this.Count > 0)
			{
				StringList savedTags = new StringList();
				try
				{
					savedTags.Duplicates = StringList.TDuplicates.dupIgnore;
					savedTags.Sorted = true;

					int num = this.Count;
					for (int i = 0; i < num; i++)
					{
						savedTags.Add(this[i].Name);
					}

					if (savedTags.IndexOf("CONC") >= 0 || savedTags.IndexOf("CONT") >= 0)
					{
						int num2 = this.Count;
						for (int i = 0; i < num2; i++)
						{
							GEDCOMTag tmp = this[i];
							
							if (tmp.Name == "CONC" || tmp.Name == "CONT")
							{
								tmp.SaveToStream(stream);
							}
						}

						if (savedTags.IndexOf("CONC") >= 0)
						{
							savedTags.Delete(savedTags.IndexOf("CONC"));
						}
						if (savedTags.IndexOf("CONT") >= 0)
						{
							savedTags.Delete(savedTags.IndexOf("CONT"));
						}
					}

					int num3 = this.Count;
					for (int i = 0; i < num3; i++) {
						GEDCOMTag tmp = this[i];
						
						if (tmp.Name != "CONT" && tmp.Name != "CONC") {
							tmp.SaveToStream(stream);
						}
					}
				}
				finally
				{
                    savedTags.Dispose();
				}
			}
		}

		protected virtual void SaveValueToStream(StreamWriter stream)
		{
			string S = this.fLevel.ToString() + " " + this.fName;

			string Val = this.StringValue;
			if (!string.IsNullOrEmpty(Val)) {
				S = S + " " + Val;
			}

			stream.WriteLine(S);
		}

		public virtual void SaveToStream(StreamWriter stream)
		{
			this.SaveValueToStream(stream);
			this.SaveTagsToStream(stream);
		}

		#endregion
	}
}
