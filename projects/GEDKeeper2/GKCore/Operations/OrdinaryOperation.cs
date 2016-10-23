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
        otPersonBookmarkChange,
        otPersonPatriarchChange,
        otPersonSexChange
    }

    /// <summary>
    /// 
    /// </summary>
    public class OrdinaryOperation : CustomOperation
    {
        private readonly OperationType fType;
        private readonly GEDCOMObject fObj;
        private readonly object fVal;

        public OrdinaryOperation(UndoManager manager, OperationType type,
                                 GEDCOMObject obj, object val) : base(manager)
        {
            this.fType = type;
            this.fObj = obj;
            this.fVal = val;
        }

        public override bool Redo()
        {
            return this.ProcessOperation(true);
        }

        public override void Undo()
        {
            this.ProcessOperation(false);
        }

        private bool ProcessOperation(bool redo)
        {
            bool result;

            switch (this.fType) {
                case OperationType.otNOP:
                    result = false;
                    break;

                case OperationType.otPersonParentsAttach:
                    {
                        GEDCOMIndividualRecord iRec = this.fObj as GEDCOMIndividualRecord;
                        GEDCOMFamilyRecord familyRec = this.fVal as GEDCOMFamilyRecord;

                        if (iRec == null || familyRec == null) {
                            result = false;
                        } else {
                            if (redo) {
                                familyRec.AddChild(iRec);
                            } else {
                                familyRec.RemoveChild(iRec);
                            }
                            result = true;
                        }
                    }
                    break;

                case OperationType.otPersonParentsDetach:
                    {
                        GEDCOMIndividualRecord iRec = this.fObj as GEDCOMIndividualRecord;
                        GEDCOMFamilyRecord familyRec = this.fVal as GEDCOMFamilyRecord;

                        if (iRec == null || familyRec == null) {
                            result = false;
                        } else {
                            if (redo) {
                                familyRec.RemoveChild(iRec);
                            } else {
                                familyRec.AddChild(iRec);
                            }
                            result = true;
                        }
                    }
                    break;

                case OperationType.otFamilySpouseAttach:
                    {
                        GEDCOMFamilyRecord famRec = this.fObj as GEDCOMFamilyRecord;
                        GEDCOMIndividualRecord spouseRec = this.fVal as GEDCOMIndividualRecord;

                        if (famRec == null || spouseRec == null) {
                            result = false;
                        } else {
                            if (redo) {
                                famRec.AddSpouse(spouseRec);
                            } else {
                                famRec.RemoveSpouse(spouseRec);
                            }
                            result = true;
                        }
                    }
                    break;

                case OperationType.otFamilySpouseDetach:
                    {
                        GEDCOMFamilyRecord famRec = this.fObj as GEDCOMFamilyRecord;
                        GEDCOMIndividualRecord spouseRec = this.fVal as GEDCOMIndividualRecord;

                        if (famRec == null || spouseRec == null) {
                            result = false;
                        } else {
                            if (redo) {
                                famRec.RemoveSpouse(spouseRec);
                            } else {
                                famRec.AddSpouse(spouseRec);
                            }
                            result = true;
                        }
                    }
                    break;

                case OperationType.otGroupMemberAttach:
                    {
                        GEDCOMGroupRecord grpRec = this.fObj as GEDCOMGroupRecord;
                        GEDCOMIndividualRecord mbrRec = this.fVal as GEDCOMIndividualRecord;

                        if (grpRec == null || mbrRec == null) {
                            result = false;
                        } else {
                            if (redo) {
                                grpRec.AddMember(mbrRec);
                            } else {
                                grpRec.RemoveMember(mbrRec);
                            }
                            result = true;
                        }
                    }
                    break;

                case OperationType.otGroupMemberDetach:
                    {
                        GEDCOMGroupRecord grpRec = this.fObj as GEDCOMGroupRecord;
                        GEDCOMIndividualRecord mbrRec = this.fVal as GEDCOMIndividualRecord;

                        if (grpRec == null || mbrRec == null) {
                            result = false;
                        } else {
                            if (redo) {
                                grpRec.RemoveMember(mbrRec);
                            } else {
                                grpRec.AddMember(mbrRec);
                            }
                            result = true;
                        }
                    }
                    break;

                case OperationType.otPersonBookmarkChange:
                    result = false;
                    break;

                case OperationType.otPersonPatriarchChange:
                    result = false;
                    break;

                case OperationType.otPersonSexChange:
                    result = false;
                    break;

                default:
                    result = false;
                    break;
            }

            return result;
        }
    }
}
