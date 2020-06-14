/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2019 by Sergey V. Zhdanovskih.
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
using System.Collections.Generic;
using BSLib;
using GDModel.Providers.GEDCOM;
using GKCore.Types;

namespace GDModel
{
    /// <summary>
    /// The main class of the entire storage infrastructure in the GEDCOM format.
    /// The ancestor of all structural classes. Contains the main logic
    /// for reading and writing the values of tags in the terminology and
    /// according to the rules of GEDCOM.
    /// </summary>
    public class GDMTag : GDMObject
    {
        #region Protected fields

        private int fId;
        private GDMObject fOwner;
        protected string fStringValue;
        private GDMList<GDMTag> fTags;

        #endregion

        #region Public properties

        public int Id
        {
            get { return fId; }
        }

        public GDMObject Owner
        {
            get { return fOwner; }
        }

        public string StringValue
        {
            get { return GetStringValue(); }
            set { ParseString(value); }
        }

        /// <summary>
        /// Access to items of nested sub-tags.
        /// </summary>
        public GDMList<GDMTag> SubTags
        {
            get { return fTags; }
        }

        #endregion

        #region Object management

        public static GDMTag Create(GDMObject owner, int tagId, string tagValue)
        {
            return new GDMTag(owner, tagId, tagValue);
        }

        public GDMTag(GDMObject owner)
        {
            fId = 0; // Unknown
            fOwner = owner;
            fTags = new GDMList<GDMTag>(this);
            fStringValue = string.Empty;
        }

        public GDMTag(GDMObject owner, int tagId, string tagValue) : this(owner)
        {
            SetNameValue(tagId, tagValue);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (fTags != null) {
                    fTags.Dispose();
                    fTags = null;
                }
            }
            base.Dispose(disposing);
        }

        public void ResetOwner(GDMObject owner)
        {
            fOwner = owner;
        }

        public virtual void ReplaceXRefs(GDMXRefReplacer map)
        {
            fTags.ReplaceXRefs(map);
        }

        public void SetNameValue(int tagId, string tagValue)
        {
            if (tagId != 0) {
                SetName(tagId);
            }

            if (!string.IsNullOrEmpty(tagValue)) {
                ParseString(tagValue);
            }
        }

        #endregion

        #region Content management

        public virtual GDMTree GetTree()
        {
            GDMTree owner = null;

            GDMTag current = this;
            while (current != null) {
                GDMObject parent = current.fOwner;

                var parentTag = parent as GDMTag;
                if (parentTag != null) {
                    current = parentTag;
                } else {
                    var parentTree = parent as GDMTree;
                    if (parentTree != null) {
                        owner = parentTree;
                    }
                    break;
                }
            }

            return owner;
        }

        public void SetName(int tagId)
        {
            fId = tagId;
        }

        public void SetName(GEDCOMTagType tag)
        {
            SetName((int)tag);
        }

        public GDMTag AddTag(GDMTag tag)
        {
            fTags.Add(tag);
            return tag;
        }

        /// <summary>
        /// Copying the sub-tags from the source to the current tag.
        /// </summary>
        /// <param name="source">A source tag.</param>
        public virtual void Assign(GDMTag source)
        {
            if (source == null) return;

            SetName(source.fId);
            ParseString(source.StringValue);

            AssignList(source.fTags, this.fTags);
        }

        protected void AssignList<T>(GDMList<T> srcList, GDMList<T> destList) where T : GDMTag
        {
            for (int i = 0, count = srcList.Count; i < count; i++) {
                GDMTag sourceTag = srcList[i];
                T copyTag = (T)Activator.CreateInstance(sourceTag.GetType(), new object[] { this });
                copyTag.Assign(sourceTag);
                destList.Add(copyTag);
            }
        }

        public virtual void Clear()
        {
            fTags.Clear();
            fStringValue = string.Empty;
        }

        public void DeleteTag(string tagName)
        {
            GDMTag tag = FindTag(tagName, 0);
            while (tag != null) {
                int idx = fTags.IndexOf(tag);
                fTags.DeleteAt(idx);

                tag = FindTag(tagName, idx);
            }
        }

        public GDMTag FindTag(string tagName, int startIndex)
        {
            string su = GEDCOMUtils.InvariantTextInfo.ToUpper(tagName);

            int pos = su.IndexOf('\\');
            string S = (pos >= 0) ? su.Substring(0, pos) : su;

            GDMTag tempTag = this;

            while (true) {
                int index = (S == su) ? startIndex : 0;

                GDMList<GDMTag> tempSubTags = tempTag.fTags;
                int tempSubCount = tempSubTags.Count;
                while (index < tempSubCount && tempSubTags[index].GetTagName() != S) index++;
                if (index >= tempSubCount) break;
                tempTag = tempSubTags[index];

                pos = su.IndexOf('\\');
                if (pos >= 0) {
                    su = su.Substring(pos + 1);

                    pos = su.IndexOf('\\');
                    S = (pos >= 0) ? su.Substring(0, pos) : su;
                } else {
                    su = "";
                }

                if (su == "") return tempTag;
            }

            return null;
        }

        public IList<GDMTag> FindTags(string tagName)
        {
            IList<GDMTag> result = new List<GDMTag>();

            GDMTag tag = FindTag(tagName, 0);
            while (tag != null) {
                int idx = fTags.IndexOf(tag);
                result.Add(tag);

                tag = FindTag(tagName, idx + 1);
            }

            return result;
        }

        public virtual bool IsEmpty()
        {
            return string.IsNullOrEmpty(fStringValue) && (fTags.Count == 0);
        }

        public virtual float IsMatch(GDMTag tag, MatchParams matchParams)
        {
            return 0.0f;
        }

        protected virtual float GetStrMatch(string str1, string str2, MatchParams matchParams)
        {
            float match = 0.0f;

            if (matchParams.NamesIndistinctThreshold >= 0.99f) {
                if (string.Compare(str1, str2, true) == 0) {
                    match = 100.0f;
                }
            } else {
                double sim = IndistinctMatching.GetSimilarity(str1, str2);
                if (sim >= matchParams.NamesIndistinctThreshold) {
                    match = 100.0f;
                }
            }

            return match;
        }

        protected virtual string GetStringValue()
        {
            return fStringValue;
        }

        public virtual string ParseString(string strValue)
        {
            fStringValue = strValue;
            return string.Empty;
        }

        #endregion
    }
}
