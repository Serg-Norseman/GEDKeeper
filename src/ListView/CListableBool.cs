/* CListableBool.cs
 * 
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
 *
 *
 * History:  
 * 10Dec08 AlexC          Migrated from GEDmill 1.10
 *
 */

using System;
using System.Windows.Forms;
using GKCommon.GEDCOM;

namespace GEDmill.ListView
{
    // Special class of ListViewItem can represent individual/source records.
    public class CListableBool : ListViewItem
    {
        // The record associated with this list item
        protected GEDCOMRecord fRecord;

        // Surname of individual
        protected string fSurname;

        // Firstname of individual
        protected string fFirstName;

        // True if this list item has a check box
        protected bool fThisIsACheckBox;


        // Constructor from individual record
        public CListableBool(GEDCOMIndividualRecord ir, string surname, string firstname, bool bThisIsACheckBox)
        {
            fRecord = ir;
            fSurname = surname;
            fFirstName = firstname;
            fThisIsACheckBox = bThisIsACheckBox;

            base.Text = ToString();
        }

        // Constructor
        public CListableBool(GEDCOMRecord isr, bool bThisIsACheckBox)
        {
            fRecord = isr;
            fSurname = "";
            fFirstName = "";
            fThisIsACheckBox = bThisIsACheckBox;

            base.Text = ToString();
        }

        // For displaying the list item
        public override string ToString()
        {
            string name = "";
            if (!fThisIsACheckBox) {
                if (fRecord is GEDCOMSourceRecord) {
                    return fRecord.ToString();
                } else {
                    if (fFirstName != "" && fSurname != "") {
                        name = string.Concat(fSurname, ", ", fFirstName);
                    } else if (fSurname != "") {
                        name = fSurname;
                    } else {
                        name = fFirstName;
                    }

                    if (name == "") {
                        name = CConfig.Instance.UnknownName;
                    }
                }
            }

            return name;
        }

        // For sorting the list
        public int CompareTo(CListableBool other)
        {
            // Assumption here is that other.m_bThisIsACheckBox will be the same (i.e. list is homogeneous)
            if (!fThisIsACheckBox) {
                // Assumption here is that other is also GEDCOMSourceRecord (i.e. list is homogeneous)
                if (fRecord is GEDCOMSourceRecord) {
                    return ((GEDCOMSourceRecord)fRecord).ShortTitle.CompareTo(((GEDCOMSourceRecord)other.fRecord).ShortTitle);
                } else {
                    if (fSurname != "" && other.fSurname != "") {
                        int result = fSurname.CompareTo(other.fSurname);
                        if (result != 0) {
                            return result;
                        }
                    } else if (fSurname != "") {
                        return 1;
                    } else if (other.fSurname != "") {
                        return -1;
                    }

                    if (fFirstName != "" && other.fFirstName != "") {
                        return fFirstName.CompareTo(other.fFirstName);
                    }

                    if (fFirstName != "") {
                        return 1;
                    }

                    if (other.fFirstName != "") {
                        return -1;
                    }

                    return 0;
                }
            } else {
                if (fRecord == null && other.fRecord == null) {
                    return 0;
                }
                if (fRecord == null) {
                    return 1;
                }
                if (other.fRecord == null) {
                    return -1;
                }
                bool tr = fRecord.GetVisibility();
                bool or = other.fRecord.GetVisibility();
                if (tr == or) {
                    return 0;
                }
                if (tr) {
                    return 1;
                }
                return -1;
            }
        }

        // Used to exclude the record from the generated web site
        public void SetRestricted(bool value)
        {
            fRecord.SetVisibility(!value);
        }

        public GEDCOMRecord Record
        {
            get {
                return fRecord;
            }
        }
    }
}
