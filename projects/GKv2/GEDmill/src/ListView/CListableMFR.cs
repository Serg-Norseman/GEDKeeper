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

using System.Windows.Forms;
using GDModel;

namespace GEDmill.ListView
{
    // Holds a multimedia file reference for use in a list box, displaying the MFRs title if present, otherwise a default string.
    public class CListableMFR : ListViewItem
    {
        // The MFR that this list item represents
        private GDMFileReferenceWithTitle m_mfr;

        // Constructor
        public CListableMFR(GDMFileReferenceWithTitle mfr)
        {
            m_mfr = mfr;

            base.Text = ToString();
        }

        // To display in list
        public override string ToString()
        {
            string desc = "";

            if (m_mfr != null) {
                if (m_mfr.Title != "") {
                    desc = m_mfr.Title;
                } else {
                    desc = "<no title>";
                }
            }

            return desc;
        }

        // Accessor
        public void SetVisible(bool bVisible)
        {
            //m_mfr.m_bVisible = bVisible;
        }

        // Accessor
        public GDMFileReference MultimediaFileReference
        {
            get {
                return m_mfr;
            }
        }
    }
}
