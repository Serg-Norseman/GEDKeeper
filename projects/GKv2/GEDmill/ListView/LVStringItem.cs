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

namespace GEDmill.ListView
{
    /// <summary>
    /// Special class of ListViewSubItem that sorts strings numerically even if they start with alphabetic character.
    /// </summary>
    public class LVStringItem : ListViewItem.ListViewSubItem, IComparable, IComparable<LVStringItem>
    {
        // The string we are encapsulating
        private string fString;

        // Constructor
        public LVStringItem(string s)
        {
            fString = s;
            base.Text = fString;
        }

        // To display list
        public override string ToString()
        {
            return fString;
        }

        public int CompareTo(object obj)
        {
            return CompareTo((LVStringItem)obj);
        }

        // Special compare will sort numerically if string is formated like "AAA111" or "I124".
        // Return -1 if this instance is less than other...
        public int CompareTo(LVStringItem other)
        {
            int nLeftL = fString.Length;
            int nRightL = other.fString.Length;
            for (int i = 0; i < nLeftL; ++i) {
                if (i >= nRightL) {
                    // left string is longer than right. right is greater.
                    return 1;
                }
                char cLeft = fString[i];
                char cRight = other.fString[i];
                if (char.IsDigit(cLeft) && char.IsDigit(cRight)) {
                    // Compare rest of strings numerically
                    int nLeft = 0;
                    int nRight = 0;
                    string sLeft = fString.Substring(i);
                    string sRight = other.fString.Substring(i);
                    bool bLeftIsNumeric = true;
                    bool bRightIsNumeric = true;
                    foreach (char c in sLeft) {
                        if (!char.IsDigit(c)) {
                            bLeftIsNumeric = false;
                            break;
                        }
                        nLeft *= 10;
                        nLeft += c - '0';
                    }
                    if (bLeftIsNumeric) {
                        foreach (char c in sRight) {
                            if (!char.IsDigit(c)) {
                                bRightIsNumeric = false;
                                break;
                            }
                            nRight *= 10;
                            nRight += c - '0';
                        }
                        if (bRightIsNumeric) {
                            return nLeft - nRight;
                        }
                    }
                }
                if (cLeft < cRight) {
                    return -1;
                } else if (cLeft > cRight) {
                    return 1;
                }
            }
            return fString.CompareTo(other.fString);
        }
    }
}
