/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using System.IO;

namespace GKCommon.GEDCOM
{
    public sealed class GEDCOMSourceRecord : GEDCOMRecord
    {
        private GEDCOMList<GEDCOMRepositoryCitation> fRepositoryCitations;

        public GEDCOMData Data
        {
            get { return base.TagClass("DATA", GEDCOMData.Create) as GEDCOMData; }
        }

        public StringList Originator
        {
            get { return base.GetTagStrings(base.TagClass("AUTH", GEDCOMTag.Create)); }
            set { base.SetTagStrings(base.TagClass("AUTH", GEDCOMTag.Create), value); }
        }

        public StringList Title
        {
            get { return base.GetTagStrings(base.TagClass("TITL", GEDCOMTag.Create)); }
            set { base.SetTagStrings(base.TagClass("TITL", GEDCOMTag.Create), value); }
        }

        public string FiledByEntry
        {
            get { return base.GetTagStringValue("ABBR"); }
            set { base.SetTagStringValue("ABBR", value); }
        }

        public StringList Publication
        {
            get { return base.GetTagStrings(base.TagClass("PUBL", GEDCOMTag.Create)); }
            set { base.SetTagStrings(base.TagClass("PUBL", GEDCOMTag.Create), value); }
        }

        public StringList Text
        {
            get { return base.GetTagStrings(base.TagClass("TEXT", GEDCOMTag.Create)); }
            set { base.SetTagStrings(base.TagClass("TEXT", GEDCOMTag.Create), value); }
        }

        public GEDCOMList<GEDCOMRepositoryCitation> RepositoryCitations
        {
            get { return this.fRepositoryCitations; }
        }

        protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
        {
            base.CreateObj(owner, parent);
            base.SetRecordType(GEDCOMRecordType.rtSource);
            base.SetName("SOUR");

            this.fRepositoryCitations = new GEDCOMList<GEDCOMRepositoryCitation>(this);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                this.fRepositoryCitations.Dispose();
            }
            base.Dispose(disposing);
        }

        public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
        {
            GEDCOMTag result;

            if (tagName == "REPO")
            {
                result = this.fRepositoryCitations.Add(new GEDCOMRepositoryCitation(base.Owner, this, tagName, tagValue));
            }
            else if (tagName == "DATA")
            {
                result = base.AddTag(tagName, tagValue, GEDCOMData.Create);
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
            this.fRepositoryCitations.Clear();
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && this.fRepositoryCitations.Count == 0;
        }

        public override void MoveTo(GEDCOMRecord targetRecord, bool clearDest)
        {
            GEDCOMSourceRecord targetSource = targetRecord as GEDCOMSourceRecord;
            if (targetSource == null)
            {
                throw new ArgumentException(@"Argument is null or wrong type", "targetRecord");
            }

            StringList titl = new StringList();
            StringList orig = new StringList();
            StringList publ = new StringList();
            StringList text = new StringList();
            try
            {
                titl.Text = (targetSource.Title.Text + "\n" + this.Title.Text).Trim();
                orig.Text = (targetSource.Originator.Text + "\n" + this.Originator.Text).Trim();
                publ.Text = (targetSource.Publication.Text + "\n" + this.Publication.Text).Trim();
                text.Text = (targetSource.Text.Text + "\n" + this.Text.Text).Trim();
                
                base.DeleteTag("TITL");
                base.DeleteTag("TEXT");
                base.DeleteTag("ABBR");
                base.DeleteTag("PUBL");
                base.DeleteTag("AUTH");

                base.MoveTo(targetRecord, clearDest);
                
                targetSource.Title = titl;
                targetSource.Originator = orig;
                targetSource.Publication = publ;
                targetSource.Text = text;

                while (this.fRepositoryCitations.Count > 0)
                {
                    GEDCOMRepositoryCitation obj = this.fRepositoryCitations.Extract(0);
                    obj.ResetParent(targetSource);
                    targetSource.RepositoryCitations.Add(obj);
                }
            }
            finally
            {
                titl.Dispose();
                orig.Dispose();
                publ.Dispose();
                text.Dispose();
            }
        }

        public override void Pack()
        {
            base.Pack();
            this.fRepositoryCitations.Pack();
        }

        public override void ReplaceXRefs(XRefReplacer map)
        {
            base.ReplaceXRefs(map);
            this.fRepositoryCitations.ReplaceXRefs(map);
        }

        public override void ResetOwner(GEDCOMTree newOwner)
        {
            base.ResetOwner(newOwner);
            this.fRepositoryCitations.ResetOwner(newOwner);
        }

        public override void SaveToStream(StreamWriter stream)
        {
            base.SaveToStream(stream);
            this.fRepositoryCitations.SaveToStream(stream);
        }


        public void SetOriginatorArray(params string[] value)
        {
            base.SetTagStrings(base.TagClass("AUTH", GEDCOMTag.Create), value);
        }

        public void SetTitleArray(params string[] value)
        {
            base.SetTagStrings(base.TagClass("TITL", GEDCOMTag.Create), value);
        }

        public void SetPublicationArray(params string[] value)
        {
            base.SetTagStrings(base.TagClass("PUBL", GEDCOMTag.Create), value);
        }

        public void SetTextArray(params string[] value)
        {
            base.SetTagStrings(base.TagClass("TEXT", GEDCOMTag.Create), value);
        }


        public GEDCOMSourceRecord(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
        {
        }

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
        {
            return new GEDCOMSourceRecord(owner, parent, tagName, tagValue);
        }

        public override float IsMatch(GEDCOMTag tag, MatchParams matchParams)
        {
            GEDCOMSourceRecord source = tag as GEDCOMSourceRecord;
            if (source == null) return 0.0f;

            float match = 0.0f;

            if (string.Compare(this.FiledByEntry, source.FiledByEntry, true) == 0)
            {
                match = 100.0f;
            }

            return match;
        }

        #region Auxiliary

        public GEDCOMRepositoryCitation AddRepository(GEDCOMRepositoryRecord repRec)
        {
            GEDCOMRepositoryCitation cit = null;
            
            if (repRec != null) {
                cit = new GEDCOMRepositoryCitation(this.Owner, this, "", "");
                cit.Value = repRec;
                this.RepositoryCitations.Add(cit);
            }
            
            return cit;
        }

        #endregion
    }
}
