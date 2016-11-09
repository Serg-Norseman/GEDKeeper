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

        otIndividualParentsAttach,
        otIndividualParentsDetach,

        otFamilySpouseAttach,
        otFamilySpouseDetach,

        otGroupMemberAttach,
        otGroupMemberDetach,

        otSourceRepositoryCitationAdd,
        otSourceRepositoryCitationRemove,

        otResearchTaskAdd,
        otResearchTaskRemove,
        otResearchCommunicationAdd,
        otResearchCommunicationRemove,
        otResearchGroupAdd,
        otResearchGroupRemove,

        otRecordNoteAdd,
        otRecordNoteRemove,

        otRecordMediaAdd,
        otRecordMediaRemove,

        otRecordSourceCitAdd,
        otRecordSourceCitRemove,

        otRecordEventAdd,
        otRecordEventRemove,

        otAdd,
        otRemove,

        otIndividualAssociationAdd,
        otIndividualAssociationRemove,

        otIndividualNameAdd,
        otIndividualNameRemove,

        otIndividualURefAdd,
        otIndividualURefRemove,

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

                case OperationType.otIndividualParentsAttach:
                case OperationType.otIndividualParentsDetach:
                    {
                        GEDCOMIndividualRecord iRec = this.fObj as GEDCOMIndividualRecord;
                        GEDCOMFamilyRecord familyRec = this.fNewVal as GEDCOMFamilyRecord;

                        if (iRec == null || familyRec == null) {
                            result = false;
                        } else {
                            if (this.fType == OperationType.otIndividualParentsDetach) {
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

                case OperationType.otSourceRepositoryCitationAdd:
                case OperationType.otSourceRepositoryCitationRemove:
                    {
                        GEDCOMSourceRecord srcRec = this.fObj as GEDCOMSourceRecord;
                        GEDCOMRepositoryRecord repRec = this.fNewVal as GEDCOMRepositoryRecord;

                        if (srcRec == null || repRec == null) {
                            result = false;
                        } else {
                            if (this.fType == OperationType.otSourceRepositoryCitationRemove) {
                                redo = !redo;
                            }
                            if (redo) {
                                srcRec.AddRepository(repRec);
                            } else {
                                srcRec.RemoveRepository(repRec);
                            }
                            result = true;
                        }
                    }
                    break;


                case OperationType.otResearchTaskAdd:
                case OperationType.otResearchTaskRemove:
                    {
                        GEDCOMResearchRecord resRec = this.fObj as GEDCOMResearchRecord;
                        GEDCOMTaskRecord taskRec = this.fNewVal as GEDCOMTaskRecord;

                        if (resRec == null || taskRec == null) {
                            result = false;
                        } else {
                            if (this.fType == OperationType.otResearchTaskRemove) {
                                redo = !redo;
                            }
                            if (redo) {
                                resRec.AddTask(taskRec);
                            } else {
                                resRec.RemoveTask(taskRec);
                            }
                            result = true;
                        }
                    }
                    break;

                case OperationType.otResearchCommunicationAdd:
                case OperationType.otResearchCommunicationRemove:
                    {
                        GEDCOMResearchRecord resRec = this.fObj as GEDCOMResearchRecord;
                        GEDCOMCommunicationRecord commRec = this.fNewVal as GEDCOMCommunicationRecord;

                        if (resRec == null || commRec == null) {
                            result = false;
                        } else {
                            if (this.fType == OperationType.otResearchCommunicationRemove) {
                                redo = !redo;
                            }
                            if (redo) {
                                resRec.AddCommunication(commRec);
                            } else {
                                resRec.RemoveCommunication(commRec);
                            }
                            result = true;
                        }
                    }
                    break;

                case OperationType.otResearchGroupAdd:
                case OperationType.otResearchGroupRemove:
                    {
                        GEDCOMResearchRecord resRec = this.fObj as GEDCOMResearchRecord;
                        GEDCOMGroupRecord grpRec = this.fNewVal as GEDCOMGroupRecord;

                        if (resRec == null || grpRec == null) {
                            result = false;
                        } else {
                            if (this.fType == OperationType.otResearchGroupRemove) {
                                redo = !redo;
                            }
                            if (redo) {
                                resRec.AddGroup(grpRec);
                            } else {
                                resRec.RemoveGroup(grpRec);
                            }
                            result = true;
                        }
                    }
                    break;


                case OperationType.otRecordNoteAdd:
                    {
                        IGEDCOMStructWithLists swl = this.fObj as IGEDCOMStructWithLists;
                        GEDCOMNoteRecord noteRec = this.fNewVal as GEDCOMNoteRecord;

                        result = (swl != null && noteRec != null);
                        if (result) {
                            if (redo) {
                                GEDCOMNotes notes = swl.AddNote(noteRec);
                                this.fOldVal = notes;
                            } else {
                                GEDCOMNotes notes = this.fOldVal as GEDCOMNotes;
                                swl.Notes.Delete(notes);
                            }
                        }
                    }
                    break;

                case OperationType.otRecordNoteRemove:
                    {
                        IGEDCOMStructWithLists swl = this.fObj as IGEDCOMStructWithLists;
                        GEDCOMNotes notes = this.fNewVal as GEDCOMNotes;

                        result = (swl != null && notes != null);
                        if (result) {
                            if (redo) {
                                swl.Notes.Extract(notes); // bugfix(no delete!)
                            } else {
                                swl.Notes.Add(notes);
                            }
                        }
                    }
                    break;

                case OperationType.otRecordMediaAdd:
                    {
                        IGEDCOMStructWithLists swl = this.fObj as IGEDCOMStructWithLists;
                        GEDCOMMultimediaRecord mediaRec = this.fNewVal as GEDCOMMultimediaRecord;

                        result = (swl != null && mediaRec != null);
                        if (result) {
                            if (redo) {
                                GEDCOMMultimediaLink mmLink = swl.AddMultimedia(mediaRec);
                                this.fOldVal = mmLink;
                            } else {
                                GEDCOMMultimediaLink mmLink = this.fOldVal as GEDCOMMultimediaLink;
                                swl.MultimediaLinks.Delete(mmLink);
                            }
                        }
                    }
                    break;

                case OperationType.otRecordMediaRemove:
                    {
                        IGEDCOMStructWithLists swl = this.fObj as IGEDCOMStructWithLists;
                        GEDCOMMultimediaLink mediaLink = this.fNewVal as GEDCOMMultimediaLink;

                        result = (swl != null && mediaLink != null);
                        if (result) {
                            if (redo) {
                                swl.MultimediaLinks.Extract(mediaLink); // bugfix(no delete!)
                            } else {
                                swl.MultimediaLinks.Add(mediaLink);
                            }
                        }
                    }
                    break;

                case OperationType.otRecordSourceCitAdd:
                case OperationType.otRecordSourceCitRemove:
                    {
                        IGEDCOMStructWithLists swl = this.fObj as IGEDCOMStructWithLists;
                        GEDCOMSourceCitation sourceCit = this.fNewVal as GEDCOMSourceCitation;

                        if (swl == null || sourceCit == null) {
                            result = false;
                        } else {
                            if (this.fType == OperationType.otRecordSourceCitRemove) {
                                redo = !redo;
                            }
                            if (redo) {
                                swl.SourceCitations.Add(sourceCit);
                            } else {
                                swl.SourceCitations.Extract(sourceCit); // bugfix(no delete!)
                            }
                            result = true;
                        }
                    }
                    break;


                case OperationType.otRecordEventAdd:
                case OperationType.otRecordEventRemove:
                    {
                        GEDCOMRecordWithEvents rwe = this.fObj as GEDCOMRecordWithEvents;
                        GEDCOMCustomEvent evt = this.fNewVal as GEDCOMCustomEvent;

                        if (rwe == null || evt == null) {
                            result = false;
                        } else {
                            if (this.fType == OperationType.otRecordEventRemove) {
                                redo = !redo;
                            }
                            if (redo) {
                                rwe.AddEvent(evt);
                            } else {
                                rwe.Events.Extract(evt); // bugfix(no delete!)
                            }
                            result = true;
                        }
                    }
                    break;


                case OperationType.otIndividualAssociationAdd:
                case OperationType.otIndividualAssociationRemove:
                    {
                        GEDCOMIndividualRecord iRec = this.fObj as GEDCOMIndividualRecord;
                        GEDCOMAssociation asso = this.fNewVal as GEDCOMAssociation;

                        if (iRec == null || asso == null) {
                            result = false;
                        } else {
                            if (this.fType == OperationType.otIndividualAssociationRemove) {
                                redo = !redo;
                            }
                            if (redo) {
                                iRec.Associations.Add(asso);
                            } else {
                                iRec.Associations.Extract(asso);
                            }
                            result = true;
                        }
                    }
                    break;

                case OperationType.otIndividualNameAdd:
                case OperationType.otIndividualNameRemove:
                    {
                        GEDCOMIndividualRecord iRec = this.fObj as GEDCOMIndividualRecord;
                        GEDCOMPersonalName persName = this.fNewVal as GEDCOMPersonalName;

                        if (iRec == null || persName == null) {
                            result = false;
                        } else {
                            if (this.fType == OperationType.otIndividualNameRemove) {
                                redo = !redo;
                            }
                            if (redo) {
                                iRec.PersonalNames.Add(persName);
                            } else {
                                iRec.PersonalNames.Extract(persName);
                            }
                            result = true;
                        }
                    }
                    break;

                case OperationType.otIndividualURefAdd:
                case OperationType.otIndividualURefRemove:
                    {
                        GEDCOMIndividualRecord iRec = this.fObj as GEDCOMIndividualRecord;
                        GEDCOMUserReference uRef = this.fNewVal as GEDCOMUserReference;

                        if (iRec == null || uRef == null) {
                            result = false;
                        } else {
                            if (this.fType == OperationType.otIndividualURefRemove) {
                                redo = !redo;
                            }
                            if (redo) {
                                iRec.UserReferences.Add(uRef);
                            } else {
                                iRec.UserReferences.Extract(uRef);
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
