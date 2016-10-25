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
using GKCommon.GEDCOM;

namespace GKCore.Operations
{
    public enum OperationType
    {
        otNOP,
        otPersonParentsAttach,
        otPersonParentsDetach,
        otFamilySpouseAttach,
        otFamilySpouseDetach,
        otGroupMemberAttach,
        otGroupMemberDetach,
        otIndividualBookmarkChange,
        otIndividualPatriarchChange,
        otIndividualSexChange
    }

    /// <summary>
    /// 
    /// </summary>
    public class OrdinaryOperation : CustomOperation
    {
        private readonly OperationType fType;
        private readonly GEDCOMObject fObj;
        private object fOldVal;
        private readonly object fNewVal;

        public OrdinaryOperation(UndoManager manager, OperationType type,
                                 GEDCOMObject obj, object newVal) : base(manager)
        {
            this.fType = type;
            this.fObj = obj;
            this.fNewVal = newVal;
        }

        public override bool Redo()
        {
            return this.ProcessOperation(true);
        }

        public override void Undo()
        {
            this.ProcessOperation(false);
        }

        /*public GEDCOMRecord FindRecord(string xref)
        {
            return this.fManager.Tree.XRefIndex_Find(xref);
        }*/

        private bool ProcessOperation(bool redo)
        {
            bool result;

            switch (this.fType) {
                case OperationType.otNOP:
                    result = false;
                    break;

                case OperationType.otPersonParentsAttach:
                case OperationType.otPersonParentsDetach:
                    {
                        GEDCOMIndividualRecord iRec = this.fObj as GEDCOMIndividualRecord;
                        GEDCOMFamilyRecord familyRec = this.fNewVal as GEDCOMFamilyRecord;

                        if (iRec == null || familyRec == null) {
                            result = false;
                        } else {
                            if (this.fType == OperationType.otPersonParentsDetach) {
                                redo = !redo;
                            }
                            if (redo) {
                                familyRec.AddChild(iRec);
                            } else {
                                familyRec.RemoveChild(iRec);
                            }
                            result = true;
                        }
                    }
                    break;

                case OperationType.otFamilySpouseAttach:
                case OperationType.otFamilySpouseDetach:
                    {
                        GEDCOMFamilyRecord famRec = this.fObj as GEDCOMFamilyRecord;
                        GEDCOMIndividualRecord spouseRec = this.fNewVal as GEDCOMIndividualRecord;

                        if (famRec == null || spouseRec == null) {
                            result = false;
                        } else {
                            if (this.fType == OperationType.otFamilySpouseDetach) {
                                redo = !redo;
                            }
                            if (redo) {
                                famRec.AddSpouse(spouseRec);
                            } else {
                                famRec.RemoveSpouse(spouseRec);
                            }
                            result = true;
                        }
                    }
                    break;

                case OperationType.otGroupMemberAttach:
                case OperationType.otGroupMemberDetach:
                    {
                        GEDCOMGroupRecord grpRec = this.fObj as GEDCOMGroupRecord;
                        GEDCOMIndividualRecord mbrRec = this.fNewVal as GEDCOMIndividualRecord;

                        if (grpRec == null || mbrRec == null) {
                            result = false;
                        } else {
                            if (this.fType == OperationType.otGroupMemberDetach) {
                                redo = !redo;
                            }
                            if (redo) {
                                grpRec.AddMember(mbrRec);
                            } else {
                                grpRec.RemoveMember(mbrRec);
                            }
                            result = true;
                        }
                    }
                    break;


                case OperationType.otIndividualBookmarkChange:
                    {
                        GEDCOMIndividualRecord iRec = this.fObj as GEDCOMIndividualRecord;

                        if (iRec == null || this.fNewVal == null) {
                            result = false;
                        } else {
                            if (redo) {
                                this.fOldVal = iRec.Bookmark;
                                iRec.Bookmark = (bool) this.fNewVal;
                            } else {
                                iRec.Bookmark = (bool) this.fOldVal;
                            }
                            result = true;
                        }
                    }
                    break;


                case OperationType.otIndividualPatriarchChange:
                    {
                        GEDCOMIndividualRecord iRec = this.fObj as GEDCOMIndividualRecord;

                        if (iRec == null || this.fNewVal == null) {
                            result = false;
                        } else {
                            if (redo) {
                                this.fOldVal = iRec.Patriarch;
                                iRec.Patriarch = (bool) this.fNewVal;
                            } else {
                                iRec.Patriarch = (bool) this.fOldVal;
                            }
                            result = true;
                        }
                    }
                    break;

                case OperationType.otIndividualSexChange:
                    {
                        GEDCOMIndividualRecord iRec = this.fObj as GEDCOMIndividualRecord;

                        if (iRec == null || this.fNewVal == null) {
                            result = false;
                        } else {
                            if (redo) {
                                this.fOldVal = iRec.Sex;
                                iRec.Sex = (GEDCOMSex) this.fNewVal;
                            } else {
                                iRec.Sex = (GEDCOMSex) this.fOldVal;
                            }
                            result = true;
                        }
                    }
                    break;

                default:
                    result = false;
                    break;
            }

            return result;
        }
    }
}
