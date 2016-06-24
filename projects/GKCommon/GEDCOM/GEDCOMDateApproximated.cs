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

namespace GKCommon.GEDCOM
{
    public sealed class GEDCOMDateApproximated : GEDCOMDate
    {
        private GEDCOMApproximated fDateApproximated;

        public GEDCOMApproximated Approximated
        {
            get { return this.fDateApproximated; }
            set { this.fDateApproximated = value; }
        }

        protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
        {
            base.CreateObj(owner, parent);
            this.fDateApproximated = GEDCOMApproximated.daExact;
        }

        protected override string GetStringValue()
        {
            string prefix;
            if (this.fDateApproximated == GEDCOMApproximated.daExact) {
                prefix = "";
            } else {
                prefix = GEDCOMDateApproximatedArray[(int)this.fDateApproximated];
                prefix += " ";
            }

            return prefix + base.GetStringValue();
        }

        private string ExtractApproximated(string str)
        {
            string result = str;
            string su = result.Substring(0, 3).ToUpperInvariant();

            for (GEDCOMApproximated i = GEDCOMApproximated.daAbout; i <= GEDCOMApproximated.daEstimated; i++)
            {
                if (su == GEDCOMDateApproximatedArray[(int)i])
                {
                    this.fDateApproximated = i;
                    result = result.Remove(0, 3);
                    break;
                }
            }

            return result;
        }

        public override string ParseString(string strValue)
        {
            string result = GEDCOMUtils.ExtractDelimiter(strValue, 0);
            result = this.ExtractApproximated(result);
            result = GEDCOMUtils.ExtractDelimiter(result, 0);
            return base.ParseString(result);
        }

        public GEDCOMDateApproximated(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
        {
        }

        public override UDN GetUDN()
        {
            return UDN.CreateApproximate(base.GetUDN());
        }
    }
}
