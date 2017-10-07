/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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

        otIndividualAssociationAdd,
        otIndividualAssociationRemove,

        otIndividualNameAdd,
        otIndividualNameRemove,

        otIndividualURefAdd,
        otIndividualURefRemove,

        otIndividualPortraitAttach,
        otIndividualPortraitDetach,

        otIndividualBookmarkChange,
        otIndividualPatriarchChange,
        otIndividualSexChange
    }

    /// <summary>
    /// Processing operations of change one of the properties of the records.
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
            fType = type;
            fObj = obj;
            fNewVal = newVal;
        }

        public override bool Redo()
        {
            return ProcessOperation(true);
        }

        public override void Undo()
        {
            ProcessOperation(false);
        }

        private bool ProcessOperation(bool redo)
        {
            bool result;

            switch (fType) {
                case OperationType.otNOP:
                    result = false;
                    break;

                case OperationType.otIndividualParentsAttach:
                case OperationType.otIndividualParentsDetach:
                    result = ProcessIndividualParents(redo);
                    break;

                case OperationType.otFamilySpouseAttach:
                case OperationType.otFamilySpouseDetach:
                    result = ProcessFamilySpouse(redo);
                    break;

                case OperationType.otGroupMemberAttach:
                case OperationType.otGroupMemberDetach:
                    result = ProcessGroupMember(redo);
                    break;

                case OperationType.otSourceRepositoryCitationAdd:
                case OperationType.otSourceRepositoryCitationRemove:
                    result = ProcessSourceRepositoryCitation(redo);
                    break;


                case OperationType.otResearchTaskAdd:
                case OperationType.otResearchTaskRemove:
                    result = ProcessResearchTask(redo);
                    break;

                case OperationType.otResearchCommunicationAdd:
                case OperationType.otResearchCommunicationRemove:
                    result = ProcessResearchCommunication(redo);
                    break;

                case OperationType.otResearchGroupAdd:
                case OperationType.otResearchGroupRemove:
                    result = ProcessResearchGroup(redo);
                    break;

                case OperationType.otRecordNoteAdd:
                    result = ProcessRecordNoteAdd(redo);
                    break;

                case OperationType.otRecordNoteRemove:
                    result = ProcessRecordNoteRemove(redo);
                    break;

                case OperationType.otRecordMediaAdd:
                    result = ProcessRecordMediaAdd(redo);
                    break;

                case OperationType.otRecordMediaRemove:
                    result = ProcessRecordMediaRemove(redo);
                    break;

                case OperationType.otRecordSourceCitAdd:
                case OperationType.otRecordSourceCitRemove:
                    result = ProcessRecordSourceCit(redo);
                    break;

                case OperationType.otRecordEventAdd:
                case OperationType.otRecordEventRemove:
                    result = ProcessRecordEvent(redo);
                    break;

                case OperationType.otIndividualAssociationAdd:
                case OperationType.otIndividualAssociationRemove:
                    result = ProcessIndividualAssociation(redo);
                    break;

                case OperationType.otIndividualNameAdd:
                case OperationType.otIndividualNameRemove:
                    result = ProcessIndividualName(redo);
                    break;

                case OperationType.otIndividualURefAdd:
                case OperationType.otIndividualURefRemove:
                    result = ProcessIndividualURef(redo);
                    break;

                case OperationType.otIndividualPortraitAttach:
                case OperationType.otIndividualPortraitDetach:
                    result = ProcessIndividualPortrait(redo);
                    break;

                case OperationType.otIndividualBookmarkChange:
                    result = ProcessIndividualBookmarkChange(redo);
                    break;

                case OperationType.otIndividualPatriarchChange:
                    result = ProcessIndividualPatriarchChange(redo);
                    break;

                case OperationType.otIndividualSexChange:
                    result = ProcessIndividualSexChange(redo);
                    break;

                default:
                    result = false;
                    break;
            }

            return result;
        }

        private bool ProcessIndividualParents(bool redo)
        {
            GEDCOMIndividualRecord iRec = fObj as GEDCOMIndividualRecord;
            GEDCOMFamilyRecord familyRec = fNewVal as GEDCOMFamilyRecord;

            if (iRec == null || familyRec == null) {
                return false;
            }

            if (fType == OperationType.otIndividualParentsDetach) {
                redo = !redo;
            }
            if (redo) {
                familyRec.AddChild(iRec);
            } else {
                familyRec.RemoveChild(iRec);
            }
            return true;
        }

        private bool ProcessFamilySpouse(bool redo)
        {
            GEDCOMFamilyRecord famRec = fObj as GEDCOMFamilyRecord;
            GEDCOMIndividualRecord spouseRec = fNewVal as GEDCOMIndividualRecord;

            if (famRec == null || spouseRec == null) {
                return false;
            }

            if (fType == OperationType.otFamilySpouseDetach) {
                redo = !redo;
            }
            if (redo) {
                famRec.AddSpouse(spouseRec);
            } else {
                famRec.RemoveSpouse(spouseRec);
            }
            return true;
        }

        private bool ProcessGroupMember(bool redo)
        {
            GEDCOMGroupRecord grpRec = fObj as GEDCOMGroupRecord;
            GEDCOMIndividualRecord mbrRec = fNewVal as GEDCOMIndividualRecord;

            if (grpRec == null || mbrRec == null) {
                return false;
            }

            if (fType == OperationType.otGroupMemberDetach) {
                redo = !redo;
            }
            if (redo) {
                grpRec.AddMember(mbrRec);
            } else {
                grpRec.RemoveMember(mbrRec);
            }
            return true;
        }

        private bool ProcessSourceRepositoryCitation(bool redo)
        {
            GEDCOMSourceRecord srcRec = fObj as GEDCOMSourceRecord;
            GEDCOMRepositoryRecord repRec = fNewVal as GEDCOMRepositoryRecord;

            if (srcRec == null || repRec == null) {
                return false;
            }

            if (fType == OperationType.otSourceRepositoryCitationRemove) {
                redo = !redo;
            }
            if (redo) {
                srcRec.AddRepository(repRec);
            } else {
                srcRec.RemoveRepository(repRec);
            }
            return true;
        }

        private bool ProcessResearchTask(bool redo)
        {
            GEDCOMResearchRecord resRec = fObj as GEDCOMResearchRecord;
            GEDCOMTaskRecord taskRec = fNewVal as GEDCOMTaskRecord;

            if (resRec == null || taskRec == null) {
                return false;
            }

            if (fType == OperationType.otResearchTaskRemove) {
                redo = !redo;
            }
            if (redo) {
                resRec.AddTask(taskRec);
            } else {
                resRec.RemoveTask(taskRec);
            }
            return true;
        }

        private bool ProcessResearchCommunication(bool redo)
        {
            GEDCOMResearchRecord resRec = fObj as GEDCOMResearchRecord;
            GEDCOMCommunicationRecord commRec = fNewVal as GEDCOMCommunicationRecord;

            if (resRec == null || commRec == null) {
                return false;
            }

            if (fType == OperationType.otResearchCommunicationRemove) {
                redo = !redo;
            }
            if (redo) {
                resRec.AddCommunication(commRec);
            } else {
                resRec.RemoveCommunication(commRec);
            }
            return true;
        }

        private bool ProcessResearchGroup(bool redo)
        {
            GEDCOMResearchRecord resRec = fObj as GEDCOMResearchRecord;
            GEDCOMGroupRecord grpRec = fNewVal as GEDCOMGroupRecord;

            if (resRec == null || grpRec == null) {
                return false;
            }

            if (fType == OperationType.otResearchGroupRemove) {
                redo = !redo;
            }
            if (redo) {
                resRec.AddGroup(grpRec);
            } else {
                resRec.RemoveGroup(grpRec);
            }
            return true;
        }

        private bool ProcessRecordNoteAdd(bool redo)
        {
            IGEDCOMStructWithLists swl = fObj as IGEDCOMStructWithLists;
            GEDCOMNoteRecord noteRec = fNewVal as GEDCOMNoteRecord;

            bool result = (swl != null && noteRec != null);
            if (result) {
                if (redo) {
                    GEDCOMNotes notes = swl.AddNote(noteRec);
                    fOldVal = notes;
                } else {
                    GEDCOMNotes notes = fOldVal as GEDCOMNotes;
                    swl.Notes.Delete(notes);
                }
            }
            return result;
        }

        private bool ProcessRecordNoteRemove(bool redo)
        {
            IGEDCOMStructWithLists swl = fObj as IGEDCOMStructWithLists;
            GEDCOMNotes notes = fNewVal as GEDCOMNotes;

            bool result = (swl != null && notes != null);
            if (result) {
                if (redo) {
                    swl.Notes.Extract(notes); // bugfix(no delete!)
                } else {
                    swl.Notes.Add(notes);
                }
            }
            return result;
        }

        private bool ProcessRecordMediaAdd(bool redo)
        {
            IGEDCOMStructWithLists swl = fObj as IGEDCOMStructWithLists;
            GEDCOMMultimediaRecord mediaRec = fNewVal as GEDCOMMultimediaRecord;

            bool result = (swl != null && mediaRec != null);
            if (result) {
                if (redo) {
                    GEDCOMMultimediaLink mmLink = swl.AddMultimedia(mediaRec);
                    fOldVal = mmLink;
                } else {
                    GEDCOMMultimediaLink mmLink = fOldVal as GEDCOMMultimediaLink;
                    swl.MultimediaLinks.Delete(mmLink);
                }
            }
            return result;
        }

        private bool ProcessRecordMediaRemove(bool redo)
        {
            IGEDCOMStructWithLists swl = fObj as IGEDCOMStructWithLists;
            GEDCOMMultimediaLink mediaLink = fNewVal as GEDCOMMultimediaLink;

            bool result = (swl != null && mediaLink != null);
            if (result) {
                if (redo) {
                    swl.MultimediaLinks.Extract(mediaLink); // bugfix(no delete!)
                } else {
                    swl.MultimediaLinks.Add(mediaLink);
                }
            }
            return result;
        }

        private bool ProcessRecordSourceCit(bool redo)
        {
            IGEDCOMStructWithLists swl = fObj as IGEDCOMStructWithLists;
            GEDCOMSourceCitation sourceCit = fNewVal as GEDCOMSourceCitation;

            if (swl == null || sourceCit == null) {
                return false;
            }

            if (fType == OperationType.otRecordSourceCitRemove) {
                redo = !redo;
            }
            if (redo) {
                swl.SourceCitations.Add(sourceCit);
            } else {
                swl.SourceCitations.Extract(sourceCit); // bugfix(no delete!)
            }
            return true;
        }

        private bool ProcessRecordEvent(bool redo)
        {
            GEDCOMRecordWithEvents rwe = fObj as GEDCOMRecordWithEvents;
            GEDCOMCustomEvent evt = fNewVal as GEDCOMCustomEvent;

            if (rwe == null || evt == null) {
                return false;
            }

            if (fType == OperationType.otRecordEventRemove) {
                redo = !redo;
            }
            if (redo) {
                rwe.AddEvent(evt);
            } else {
                rwe.Events.Extract(evt); // bugfix(no delete!)
            }
            return true;
        }

        private bool ProcessIndividualAssociation(bool redo)
        {
            GEDCOMIndividualRecord iRec = fObj as GEDCOMIndividualRecord;
            GEDCOMAssociation asso = fNewVal as GEDCOMAssociation;

            if (iRec == null || asso == null) {
                return false;
            }

            if (fType == OperationType.otIndividualAssociationRemove) {
                redo = !redo;
            }
            if (redo) {
                iRec.Associations.Add(asso);
            } else {
                iRec.Associations.Extract(asso);
            }
            return true;
        }

        private bool ProcessIndividualName(bool redo)
        {
            GEDCOMIndividualRecord iRec = fObj as GEDCOMIndividualRecord;
            GEDCOMPersonalName persName = fNewVal as GEDCOMPersonalName;

            if (iRec == null || persName == null) {
                return false;
            }

            if (fType == OperationType.otIndividualNameRemove) {
                redo = !redo;
            }
            if (redo) {
                iRec.PersonalNames.Add(persName);
            } else {
                iRec.PersonalNames.Extract(persName);
            }
            return true;
        }

        private bool ProcessIndividualURef(bool redo)
        {
            GEDCOMIndividualRecord iRec = fObj as GEDCOMIndividualRecord;
            GEDCOMUserReference uRef = fNewVal as GEDCOMUserReference;

            if (iRec == null || uRef == null) {
                return false;
            }

            if (fType == OperationType.otIndividualURefRemove) {
                redo = !redo;
            }
            if (redo) {
                iRec.UserReferences.Add(uRef);
            } else {
                iRec.UserReferences.Extract(uRef);
            }
            return true;
        }

        private bool ProcessIndividualPortrait(bool redo)
        {
            GEDCOMIndividualRecord iRec = fObj as GEDCOMIndividualRecord;
            GEDCOMMultimediaLink mmNewLink = fNewVal as GEDCOMMultimediaLink;
            GEDCOMMultimediaLink mmOldLink = fOldVal as GEDCOMMultimediaLink;

            if (iRec == null || mmNewLink == null) {
                return false;
            }

            if (fType == OperationType.otIndividualPortraitDetach) {
                redo = !redo;
            }

            if (redo) {
            } else {
            }

            return true;
        }

        /// <summary>
        /// Processing of undo/redo operations bookmark change of personal records.
        /// </summary>
        /// <param name="redo"></param>
        /// <returns></returns>
        private bool ProcessIndividualBookmarkChange(bool redo)
        {
            GEDCOMIndividualRecord iRec = fObj as GEDCOMIndividualRecord;

            if (iRec == null || fNewVal == null) {
                return false;
            }

            if (redo) {
                fOldVal = iRec.Bookmark;
                iRec.Bookmark = (bool) fNewVal;
            } else {
                iRec.Bookmark = (bool) fOldVal;
            }
            return true;
        }

        /// <summary>
        /// Processing of undo/redo operations patriarch's bookmark change of personal records.
        /// </summary>
        /// <param name="redo"></param>
        /// <returns></returns>
        private bool ProcessIndividualPatriarchChange(bool redo)
        {
            GEDCOMIndividualRecord iRec = fObj as GEDCOMIndividualRecord;

            if (iRec == null || fNewVal == null) {
                return false;
            }

            if (redo) {
                fOldVal = iRec.Patriarch;
                iRec.Patriarch = (bool) fNewVal;
            } else {
                iRec.Patriarch = (bool) fOldVal;
            }
            return true;
        }

        /// <summary>
        /// Processing of undo/redo operations sex change of personal records.
        /// </summary>
        /// <param name="redo"></param>
        /// <returns></returns>
        private bool ProcessIndividualSexChange(bool redo)
        {
            GEDCOMIndividualRecord iRec = fObj as GEDCOMIndividualRecord;

            if (iRec == null || fNewVal == null) {
                return false;
            }

            if (redo) {
                fOldVal = iRec.Sex;
                iRec.Sex = (GEDCOMSex) fNewVal;
            } else {
                iRec.Sex = (GEDCOMSex) fOldVal;
            }
            return true;
        }
    }
}
