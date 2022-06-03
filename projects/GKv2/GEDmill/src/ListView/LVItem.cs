/* 
 * Copyright 2009 Alexander Curtis <alex@logicmill.com>
 * This file is part of GEDmill - A family history website creator
 * 
 * GEDmill is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * GEDmill is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GEDmill.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using System.Windows.Forms;
using GDModel;

namespace GEDmill.ListView
{
    // Special class of ListViewItem can represent individual/source records.
    public class LVItem : ListViewItem, IComparable, IComparable<LVItem>
    {
        // The record associated with this list item
        protected GDMRecord fRecord;

        // True if this list item has a check box
        protected bool fCheckBox;


        // Constructor from record
        public LVItem(GDMRecord ir)
        {
            fRecord = ir;
            fCheckBox = true;
            base.Text = ToString();
        }

        // For displaying the list item
        public override string ToString()
        {
            return string.Empty;
        }

        public int CompareTo(object obj)
        {
            return CompareTo((LVItem)obj);
        }

        // For sorting the list
        public int CompareTo(LVItem other)
        {
            if (fRecord == null && other.fRecord == null) {
                return 0;
            }
            if (fRecord == null) {
                return 1;
            }
            if (other.fRecord == null) {
                return -1;
            }
            bool tr = GMHelper.GetVisibility(fRecord);
            bool or = GMHelper.GetVisibility(other.fRecord);
            return (tr == or) ? 0 : (tr) ? 1 : -1;
        }

        // Used to exclude the record from the generated web site
        public void SetVisibility(bool value)
        {
            GMHelper.SetVisibility(fRecord, value);
        }

        public GDMRecord Record
        {
            get { return fRecord; }
        }
    }
}
