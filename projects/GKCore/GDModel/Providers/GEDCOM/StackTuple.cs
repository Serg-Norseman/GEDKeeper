using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
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
namespace GDModel.Providers.GEDCOM
{
    public sealed class StackTuple
    {
        public int Level { get; set; }
        public GDMTag Tag { get; set; }
        public AddTagHandler AddHandler { get; set; }

        public StackTuple(int level, GDMTag tag, AddTagHandler addHandler = null)
        {
            Level = level;
            Tag = tag;
            AddHandler = addHandler ?? GetAddHandler(tag.Id);
        }
        internal static StackTuple AddTreeTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMTree tree = (GDMTree)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;
            GEDCOMTagType tagType = (GEDCOMTagType)tagId;

            if (tagType == GEDCOMTagType.INDI)
            {
                curTag = tree.AddRecord(new GDMIndividualRecord(tree));
                addHandler = AddIndividualRecordTag;

            }
            else if (tagType == GEDCOMTagType.FAM)
            {
                curTag = tree.AddRecord(new GDMFamilyRecord(tree));
                addHandler = AddFamilyRecordTag;

            }
            else if (tagType == GEDCOMTagType.OBJE)
            {
                curTag = tree.AddRecord(new GDMMultimediaRecord(tree));
                addHandler = AddMultimediaRecordTag;

            }
            else if (tagType == GEDCOMTagType.NOTE)
            {
                curTag = tree.AddRecord(new GDMNoteRecord(tree));
                curTag.ParseString(tagValue);
                addHandler = AddNoteRecordTag;

            }
            else if (tagType == GEDCOMTagType.REPO)
            {
                curTag = tree.AddRecord(new GDMRepositoryRecord(tree));
                addHandler = AddRepositoryRecordTag;

            }
            else if (tagType == GEDCOMTagType.SOUR)
            {
                curTag = tree.AddRecord(new GDMSourceRecord(tree));
                addHandler = AddSourceRecordTag;

            }
            else if (tagType == GEDCOMTagType.SUBN)
            {
                curTag = tree.AddRecord(new GDMSubmissionRecord(tree));
                addHandler = AddSubmissionRecordTag;

            }
            else if (tagType == GEDCOMTagType.SUBM)
            {
                curTag = tree.AddRecord(new GDMSubmitterRecord(tree));
                addHandler = AddSubmitterRecordTag;

            }
            else if (tagType == GEDCOMTagType._GROUP)
            {
                curTag = tree.AddRecord(new GDMGroupRecord(tree));
                addHandler = AddGroupRecordTag;

            }
            else if ((tagType == GEDCOMTagType._GRP) && (tree.Format == GEDCOMFormat.gf_Genney))
            {
                curTag = tree.AddRecord(new GDMGroupRecord(tree));
                addHandler = AddGroupRecordTag;

            }
            else if (tagType == GEDCOMTagType._RESEARCH)
            {
                curTag = tree.AddRecord(new GDMResearchRecord(tree));
                addHandler = AddResearchRecordTag;

            }
            else if (tagType == GEDCOMTagType._TASK)
            {
                curTag = tree.AddRecord(new GDMTaskRecord(tree));
                addHandler = AddTaskRecordTag;

            }
            else if (tagType == GEDCOMTagType._COMM)
            {
                curTag = tree.AddRecord(new GDMCommunicationRecord(tree));
                addHandler = AddCommunicationRecordTag;

            }
            else if (tagType == GEDCOMTagType._LOC)
            {
                curTag = tree.AddRecord(new GDMLocationRecord(tree));
                addHandler = AddLocationRecordTag;

            }
            else if ((tagType == GEDCOMTagType._PLAC) && (tree.Format == GEDCOMFormat.gf_FamilyHistorian))
            {
                curTag = tree.AddRecord(new GDMLocationRecord(tree));
                ((GDMLocationRecord)curTag).LocationName = tagValue;
                addHandler = AddLocationRecordTag;

            }
            else if ((tagType == GEDCOMTagType._PLC) && (tree.Format == GEDCOMFormat.gf_Genney))
            {
                curTag = tree.AddRecord(new GDMLocationRecord(tree));
                addHandler = AddLocationRecordTag;

            }
            else if (tagType == GEDCOMTagType.HEAD)
            {
                curTag = tree.Header;
                addHandler = AddHeaderTag;

            }
            else if (tagType == GEDCOMTagType.TRLR)
            {
                curTag = null;

            }
            else
            {
                curTag = tree.AddRecord(new GDMRecord(tree));
                addHandler = AddRecordTag;
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddSubmissionRecordTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMSubmissionRecord submnRec = (GDMSubmissionRecord)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.SUBM)
            {
                curTag = submnRec.Submitter;
                curTag.ParseString(tagValue);
            }
            else if (tagType == GEDCOMTagType.FAMF)
            {
                submnRec.FamilyFileName = tagValue;
            }
            else if (tagType == GEDCOMTagType.TEMP)
            {
                submnRec.TempleCode = tagValue;
            }
            else if (tagType == GEDCOMTagType.ANCE)
            {
                submnRec.GenerationsOfAncestors = GEDCOMUtils.GetIntVal(tagValue);
            }
            else if (tagType == GEDCOMTagType.DESC)
            {
                submnRec.GenerationsOfDescendants = GEDCOMUtils.GetIntVal(tagValue);
            }
            else if (tagType == GEDCOMTagType.ORDI)
            {
                submnRec.OrdinanceProcessFlag = GEDCOMUtils.GetOrdinanceProcessFlagVal(tagValue);
            }
            else
            {
                return AddRecordTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddSubmitterRecordTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMSubmitterRecord submrRec = (GDMSubmitterRecord)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.NAME)
            {
                curTag = submrRec.Name;
                curTag.ParseString(tagValue);
                addHandler = AddPersonalNameTag;
            }
            else if (tagType == GEDCOMTagType.ADDR)
            {
                curTag = submrRec.Address;
                curTag.ParseString(tagValue);
                addHandler = AddAddressTag;
            }
            else if (tagType == GEDCOMTagType.PHON || tagType == GEDCOMTagType.EMAIL || tagType == GEDCOMTagType.FAX || tagType == GEDCOMTagType.WWW)
            {
                return AddAddressTag(submrRec.Address, tagLevel, tagId, tagValue);
            }
            else if (tagType == GEDCOMTagType.LANG)
            {
                curTag = submrRec.AddLanguage(new GDMLanguage(submrRec, tagId, tagValue));
            }
            else if (tagType == GEDCOMTagType.RFN)
            {
                submrRec.RegisteredReference = tagValue;
            }
            else
            {
                // 'ADDR' defines by default
                return AddRecordTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        internal static StackTuple AddRecordTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMRecord record = (GDMRecord)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.NOTE)
            {
                curTag = record.Notes.Add(new GDMNotes(record));
                curTag.ParseString(tagValue);
                addHandler = AddNoteTag;
            }
            else if (tagType == GEDCOMTagType.SOUR)
            {
                curTag = record.SourceCitations.Add(new GDMSourceCitation(record));
                curTag.ParseString(tagValue);
                addHandler = AddSourceCitationTag;
            }
            else if (tagType == GEDCOMTagType.OBJE)
            {
                curTag = record.MultimediaLinks.Add(new GDMMultimediaLink(record));
                curTag.ParseString(tagValue);
                addHandler = AddMultimediaLinkTag;
            }
            else if (tagType == GEDCOMTagType.REFN)
            {
                curTag = record.UserReferences.Add(new GDMUserReference(record));
                curTag.ParseString(tagValue);
                addHandler = AddUserReferenceTag;
            }
            else if (tagType == GEDCOMTagType._UID)
            {
                record.UID = tagValue;
            }
            else if (tagType == GEDCOMTagType.RIN)
            {
                record.AutomatedRecordID = tagValue;
            }
            else if (tagType == GEDCOMTagType.CHAN)
            {
                curTag = record.ChangeDate;
                addHandler = AddChangeDateTag;
            }
            else
            {
                return AddBaseTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }
        private static StackTuple AddHeaderTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMHeader header = (GDMHeader)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.CHAR)
            {
                curTag = header.CharacterSet;
                curTag.ParseString(tagValue);
                addHandler = AddHeaderCharSetTag;
            }
            else if (tagType == GEDCOMTagType.SOUR)
            {
                curTag = header.Source;
                curTag.ParseString(tagValue);
                addHandler = AddHeaderSourceTag;
            }
            else if (tagType == GEDCOMTagType.GEDC)
            {
                curTag = header.GEDCOM;
                curTag.ParseString(tagValue);
                addHandler = AddHeaderGEDCOMTag;
            }
            else if (tagType == GEDCOMTagType.LANG)
            {
                header.Language = GEDCOMUtils.GetLanguageVal(tagValue);
            }
            else if (tagType == GEDCOMTagType.COPR)
            {
                header.Copyright = tagValue;
            }
            else if (tagType == GEDCOMTagType.DEST)
            {
                header.ReceivingSystemName = tagValue;
            }
            else if (tagType == GEDCOMTagType.PLAC)
            {
                curTag = header.Place;
                curTag.ParseString(tagValue);
                addHandler = AddPlaceTag;
            }
            else if (tagType == GEDCOMTagType.SUBM)
            {
                curTag = header.Submitter;
                curTag.ParseString(tagValue);
            }
            else if (tagType == GEDCOMTagType.SUBN)
            {
                curTag = header.Submission;
                curTag.ParseString(tagValue);
            }
            else if (tagType == GEDCOMTagType.NOTE)
            {
                curTag = header.Note;
                curTag.ParseString(tagValue);
                addHandler = AddTextTag;
            }
            else if (tagType == GEDCOMTagType.DATE)
            {
                DateTime date;
                GEDCOMUtils.ParseDate(header.GetTree(), tagValue, out date);
                header.TransmissionDateTime = date;
                curTag = header;
                addHandler = AddHeaderTag;
            }
            else if (tagType == GEDCOMTagType.TIME)
            {
                TimeSpan time;
                GEDCOMUtils.ParseTime(tagValue, out time);
                DateTime date = header.TransmissionDateTime;
                header.TransmissionDateTime = date.Add(time);
            }
            else if (tagType == GEDCOMTagType.FILE)
            {
                curTag = header.File;
                curTag.ParseString(tagValue);
                addHandler = AddHeaderFileTag;
            }
            else
            {
                return AddBaseTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddHeaderGEDCOMTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMHeaderGEDCOM headerGEDCOM = (GDMHeaderGEDCOM)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.VERS)
            {
                headerGEDCOM.Version = tagValue;
            }
            else if (tagType == GEDCOMTagType.FORM)
            {
                headerGEDCOM.Form = tagValue;
            }
            else
            {
                return AddBaseTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddHeaderCharSetTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMHeaderCharSet headerCharSet = (GDMHeaderCharSet)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.VERS)
            {
                headerCharSet.Version = tagValue;
            }
            else
            {
                return AddBaseTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddHeaderFileTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMHeaderFile headerFile = (GDMHeaderFile)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType._REV)
            {
                headerFile.Revision = GEDCOMUtils.GetIntVal(tagValue);
            }
            else
            {
                return AddBaseTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        internal static StackTuple AddChangeDateTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMChangeDate changeDate = (GDMChangeDate)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.DATE)
            {
                DateTime date;
                GEDCOMUtils.ParseDate(changeDate.GetTree(), tagValue, out date);
                changeDate.ChangeDateTime = date;
                curTag = changeDate;
            }
            else if (tagType == GEDCOMTagType.TIME)
            {
                TimeSpan time;
                GEDCOMUtils.ParseTime(tagValue, out time);
                DateTime date = changeDate.ChangeDateTime;
                changeDate.ChangeDateTime = date.Add(time);
            }
            else if (tagType == GEDCOMTagType.NOTE)
            {
                curTag = changeDate.AddTag(new GDMNotes(changeDate));
                curTag.ParseString(tagValue);
                addHandler = AddNoteTag;
            }
            else
            {
                return AddBaseTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        internal static StackTuple AddAddressTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMAddress addr = (GDMAddress)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.CONT)
            {
                addr.Lines.Add(tagValue);
            }
            else if (tagType == GEDCOMTagType.ADR1)
            {
                addr.AddressLine1 = tagValue;
            }
            else if (tagType == GEDCOMTagType.ADR2)
            {
                addr.AddressLine2 = tagValue;
            }
            else if (tagType == GEDCOMTagType.ADR3)
            {
                addr.AddressLine3 = tagValue;
            }
            else if (tagType == GEDCOMTagType.CITY)
            {
                addr.AddressCity = tagValue;
            }
            else if (tagType == GEDCOMTagType.STAE)
            {
                addr.AddressState = tagValue;
            }
            else if (tagType == GEDCOMTagType.POST)
            {
                addr.AddressPostalCode = tagValue;
            }
            else if (tagType == GEDCOMTagType.CTRY)
            {
                addr.AddressCountry = tagValue;
            }
            else if (tagType == GEDCOMTagType.PHON)
            {
                curTag = addr.AddPhoneNumber(tagValue);
            }
            else if (tagType == GEDCOMTagType.EMAIL)
            {
                curTag = addr.AddEmailAddress(tagValue);
            }
            else if (tagType == GEDCOMTagType.FAX)
            {
                curTag = addr.AddFaxNumber(tagValue);
            }
            else if (tagType == GEDCOMTagType.WWW)
            {
                curTag = addr.AddWebPage(tagValue);
            }
            else
            {
                return AddBaseTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        internal static StackTuple AddBaseTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMTag ownerTag = (GDMTag)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            curTag = GEDCOMProvider.CreateTag(ownerTag, tagId, tagValue);
            if (curTag == null)
            {
                curTag = new GDMTag(ownerTag, tagId, tagValue);
            }
            curTag = ownerTag.AddTag(curTag);

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        internal static StackTuple AddTextTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            IGDMTextObject textTag = (IGDMTextObject)owner;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            var strings = textTag.Lines;
            if (tagType == GEDCOMTagType.CONC)
            {
                int strCount = strings.Count;
                if (strCount > 0)
                {
                    strings[strCount - 1] = strings[strCount - 1] + tagValue;
                }
                else
                {
                    strings.Add(tagValue);
                }
            }
            else if (tagType == GEDCOMTagType.CONT)
            {
                strings.Add(tagValue);
            }

            return CreateReaderStackTuple(tagLevel, null, null);
        }

        private static StackTuple AddTagWithListsTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMTagWithLists tagWL = (GDMTagWithLists)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.NOTE)
            {
                curTag = tagWL.Notes.Add(new GDMNotes(tagWL));
                curTag.ParseString(tagValue);
                addHandler = AddNoteTag;
            }
            else if (tagType == GEDCOMTagType.SOUR)
            {
                curTag = tagWL.SourceCitations.Add(new GDMSourceCitation(tagWL));
                curTag.ParseString(tagValue);
                addHandler = AddSourceCitationTag;
            }
            else if (tagType == GEDCOMTagType.OBJE)
            {
                curTag = tagWL.MultimediaLinks.Add(new GDMMultimediaLink(tagWL));
                curTag.ParseString(tagValue);
                addHandler = AddMultimediaLinkTag;
            }
            else
            {
                return AddBaseTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddIndividualRecordTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMIndividualRecord indiRec = (GDMIndividualRecord)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.FAMC)
            {
                curTag = indiRec.ChildToFamilyLinks.Add(new GDMChildToFamilyLink(indiRec));
                curTag.ParseString(tagValue);
                addHandler = AddChildToFamilyLinkTag;
            }
            else if (tagType == GEDCOMTagType.FAMS)
            {
                curTag = indiRec.SpouseToFamilyLinks.Add(new GDMSpouseToFamilyLink(indiRec));
                curTag.ParseString(tagValue);
            }
            else if (tagType == GEDCOMTagType.NAME)
            {
                curTag = indiRec.AddPersonalName(new GDMPersonalName(indiRec));
                curTag.ParseString(tagValue);
                addHandler = AddPersonalNameTag;
            }
            else if (tagType == GEDCOMTagType.ASSO)
            {
                curTag = indiRec.Associations.Add(new GDMAssociation(indiRec));
                curTag.ParseString(tagValue);
                addHandler = AddAssociationTag;
            }
            else if (tagType == GEDCOMTagType.ALIA)
            {
                curTag = indiRec.Aliases.Add(new GDMAlias(indiRec));
                curTag.ParseString(tagValue);
            }
            else if (GEDCOMUtils.IsIndiEvent(tagType))
            {
                curTag = indiRec.AddEvent(new GDMIndividualEvent(indiRec, tagId, tagValue));
                addHandler = AddCustomEventTag;
            }
            else if (GEDCOMUtils.IsIndiAttr(tagType))
            {
                curTag = indiRec.AddEvent(new GDMIndividualAttribute(indiRec, tagId, tagValue));
                addHandler = AddCustomEventTag;
            }
            else if (tagType == GEDCOMTagType._GROUP)
            {
                curTag = indiRec.Groups.Add(new GDMPointer(indiRec, tagId, tagValue));
            }
            else if (tagType == GEDCOMTagType.SEX)
            {
                indiRec.Sex = GEDCOMUtils.GetSexVal(tagValue);
            }
            else
            {
                return AddRecordWithEventsTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddFamilyRecordTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMFamilyRecord famRec = (GDMFamilyRecord)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.HUSB)
            {
                curTag = famRec.Husband;
                curTag.ParseString(tagValue);
            }
            else if (tagType == GEDCOMTagType.WIFE)
            {
                curTag = famRec.Wife;
                curTag.ParseString(tagValue);
            }
            else if (tagType == GEDCOMTagType.CHIL)
            {
                curTag = famRec.Children.Add(new GDMIndividualLink(famRec, tagId, tagValue));
            }
            else if (tagType == GEDCOMTagType._STAT)
            {
                famRec.Status = GEDCOMUtils.GetMarriageStatusVal(tagValue);
            }
            else if (GEDCOMUtils.IsFamEvent(tagType))
            {
                curTag = famRec.AddEvent(new GDMFamilyEvent(famRec, tagId, tagValue));
                addHandler = AddCustomEventTag;
            }
            else
            {
                return AddRecordWithEventsTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddRecordWithEventsTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMRecordWithEvents evtRec = (GDMRecordWithEvents)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.RESN)
            {
                evtRec.Restriction = GEDCOMUtils.GetRestrictionVal(tagValue);
            }
            else
            {
                return AddRecordTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }
        private static StackTuple AddLocationRecordTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMLocationRecord locRec = (GDMLocationRecord)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.NAME)
            {
                locRec.LocationName = tagValue;
            }
            else if (tagType == GEDCOMTagType.MAP)
            {
                curTag = locRec.Map;
                addHandler = AddMapTag;
            }
            else
            {
                return AddRecordTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }
        internal static StackTuple AddPlaceTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMPlace place = (GDMPlace)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.FORM)
            {
                place.Form = tagValue;
            }
            else if (tagType == GEDCOMTagType.MAP)
            {
                curTag = place.Map;
                addHandler = AddMapTag;
            }
            else if (tagType == GEDCOMTagType._LOC)
            {
                curTag = place.Location;
                curTag.ParseString(tagValue);
            }
            else
            {
                return AddTagWithListsTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }
        private static StackTuple CreateReaderStackTuple(int level, GDMTag tag, AddTagHandler addHandler)
        {
            if (tag == null)
            {
                return null;
            }
            else
            {
                if (addHandler == null)
                {
                    addHandler = GetAddHandler(tag.Id);
                }
                return new StackTuple(level, tag, addHandler);
            }
        }

        internal static StackTuple AddMapTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMMap map = (GDMMap)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.LATI)
            {
                map.Lati = GEDCOMUtils.GetGeoCoord(tagValue, GeoCoord.Lati);
            }
            else if (tagType == GEDCOMTagType.LONG)
            {
                map.Long = GEDCOMUtils.GetGeoCoord(tagValue, GeoCoord.Long);
            }
            else
            {
                return AddBaseTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }
        // Format: FORM\TYPE
        private static StackTuple AddFileReferenceWithTitleTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMFileReferenceWithTitle fileRef = (GDMFileReferenceWithTitle)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.TITL)
            {
                fileRef.Title = tagValue;
            }
            else if (tagType == GEDCOMTagType.FORM)
            {
                fileRef.MultimediaFormat = GEDCOMUtils.GetMultimediaFormatVal(tagValue);
                curTag = fileRef;
                addHandler = AddFileReferenceWithTitleTag;
            }
            else if (tagType == GEDCOMTagType.TYPE)
            {
                fileRef.MediaType = GEDCOMUtils.GetMediaTypeVal(tagValue);
            }
            else
            {
                return AddBaseTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }
        internal static StackTuple AddUserReferenceTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMUserReference userRef = (GDMUserReference)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.TYPE)
            {
                userRef.ReferenceType = tagValue;
            }
            else
            {
                return AddBaseTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        // Format: FORM\MEDI
        private static StackTuple AddFileReferenceTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMFileReference fileRef = (GDMFileReference)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.FORM)
            {
                fileRef.MultimediaFormat = GEDCOMUtils.GetMultimediaFormatVal(tagValue);
                curTag = fileRef;
                addHandler = AddFileReferenceTag;
            }
            else if (tagType == GEDCOMTagType.MEDI)
            {
                fileRef.MediaType = GEDCOMUtils.GetMediaTypeVal(tagValue);
            }
            else
            {
                return AddBaseTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddMultimediaLinkTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMMultimediaLink mmLink = (GDMMultimediaLink)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.TITL)
            {
                mmLink.Title = tagValue;
            }
            else if (tagType == GEDCOMTagType._PRIM)
            {
                mmLink.IsPrimary = GEDCOMUtils.GetBoolVal(tagValue);
            }
            else if (tagType == GEDCOMTagType._PRIM_CUTOUT)
            {
                mmLink.IsPrimaryCutout = GEDCOMUtils.GetBoolVal(tagValue);
            }
            else if (tagType == GEDCOMTagType._POSITION)
            {
                curTag = mmLink.CutoutPosition;
                curTag.ParseString(tagValue);
            }
            else if (tagType == GEDCOMTagType.FILE)
            {
                curTag = mmLink.FileReferences.Add(new GDMFileReference(mmLink));
                curTag.ParseString(tagValue);
                addHandler = AddFileReferenceTag;
            }
            else
            {
                return AddBaseTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddNoteTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMNotes note = (GDMNotes)owner;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (IsTextTag(tagType))
            {
                return AddTextTag(note, tagLevel, tagId, tagValue);
            }
            else
            {
                return AddBaseTag(note, tagLevel, tagId, tagValue);
            }
        }

        private static StackTuple AddSourceCitationTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMSourceCitation sourCit = (GDMSourceCitation)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (IsTextTag(tagType))
            {
                return AddTextTag(sourCit, tagLevel, tagId, tagValue);
            }
            else if (tagType == GEDCOMTagType.QUAY)
            {
                sourCit.CertaintyAssessment = GEDCOMUtils.GetIntVal(tagValue);
            }
            else if (tagType == GEDCOMTagType.PAGE)
            {
                sourCit.Page = tagValue;
            }
            else
            {
                return AddBaseTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        internal static StackTuple AddAssociationTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMAssociation asso = (GDMAssociation)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.RELA)
            {
                asso.Relation = tagValue;
            }
            else if (tagType == GEDCOMTagType.SOUR)
            {
                curTag = asso.SourceCitations.Add(new GDMSourceCitation(asso));
                curTag.ParseString(tagValue);
                addHandler = AddSourceCitationTag;
            }
            else
            {
                return AddPointerWithNotesTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        internal static StackTuple AddChildToFamilyLinkTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMChildToFamilyLink cfl = (GDMChildToFamilyLink)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.STAT)
            {
                cfl.ChildLinkageStatus = GEDCOMUtils.GetChildLinkageStatusVal(tagValue);
            }
            else if (tagType == GEDCOMTagType.PEDI)
            {
                cfl.PedigreeLinkageType = GEDCOMUtils.GetPedigreeLinkageTypeVal(tagValue);
            }
            else
            {
                return AddPointerWithNotesTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        internal static StackTuple AddPersonalNameTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMPersonalName persName = (GDMPersonalName)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.TYPE)
            {
                persName.NameType = GEDCOMUtils.GetNameTypeVal(tagValue);
            }
            else if (tagType == GEDCOMTagType._LANG || tagType == GEDCOMTagType.LANG)
            {
                persName.Language = GEDCOMUtils.GetLanguageVal(tagValue);
            }
            else if (tagType == GEDCOMTagType.FONE || tagType == GEDCOMTagType.ROMN)
            {
                return AddBaseTag(owner, tagLevel, tagId, tagValue);
            }
            else
            {
                return AddPersonalNamePiecesTag(persName.Pieces, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddPersonalNamePiecesTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMPersonalNamePieces persNamePieces = (GDMPersonalNamePieces)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.NPFX)
            {
                persNamePieces.Prefix = tagValue;
            }
            else if (tagType == GEDCOMTagType.GIVN)
            {
                persNamePieces.Given = tagValue;
            }
            else if (tagType == GEDCOMTagType.NICK)
            {
                persNamePieces.Nickname = tagValue;
            }
            else if (tagType == GEDCOMTagType.SPFX)
            {
                persNamePieces.SurnamePrefix = tagValue;
            }
            else if (tagType == GEDCOMTagType.SURN)
            {
                persNamePieces.Surname = tagValue;
            }
            else if (tagType == GEDCOMTagType.NSFX)
            {
                persNamePieces.Suffix = tagValue;
            }
            else if (tagType == GEDCOMTagType._PATN)
            {
                persNamePieces.PatronymicName = tagValue;
            }
            else if (tagType == GEDCOMTagType._MARN)
            {
                persNamePieces.MarriedName = tagValue;
            }
            else if (tagType == GEDCOMTagType._RELN)
            {
                persNamePieces.ReligiousName = tagValue;
            }
            else if (tagType == GEDCOMTagType._CENN)
            {
                persNamePieces.CensusName = tagValue;
            }
            else
            {
                return AddTagWithListsTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }
        private static StackTuple AddSourceDataTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMSourceData sourData = (GDMSourceData)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.EVEN)
            {
                curTag = sourData.Events.Add(new GDMSourceEvent(sourData));
                curTag.ParseString(tagValue);
                addHandler = AddSourceDataEventTag;
            }
            else if (tagType == GEDCOMTagType.AGNC)
            {
                sourData.Agency = tagValue;
            }
            else
            {
                return AddTagWithListsTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }
        private static StackTuple AddSourceDataEventTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMSourceEvent dataEvent = (GDMSourceEvent)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.DATE)
            {
                curTag = dataEvent.Date;
                curTag.ParseString(tagValue);
            }
            else if (tagType == GEDCOMTagType.PLAC)
            {
                curTag = dataEvent.Place;
                curTag.ParseString(tagValue);
                addHandler = AddPlaceTag;
            }
            else
            {
                return AddBaseTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }
        internal static StackTuple AddPointerWithNotesTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMPointerWithNotes ptrWN = (GDMPointerWithNotes)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.NOTE)
            {
                curTag = ptrWN.Notes.Add(new GDMNotes(ptrWN));
                curTag.ParseString(tagValue);
                addHandler = AddNoteTag;
            }
            else
            {
                return AddBaseTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }
        internal static StackTuple AddCustomEventTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMCustomEvent custEvent = (GDMCustomEvent)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.ADDR)
            {
                curTag = custEvent.Address;
                curTag.ParseString(tagValue);
                addHandler = AddAddressTag;
            }
            else if (tagType == GEDCOMTagType.AGNC)
            {
                custEvent.Agency = tagValue;
            }
            else if (tagType == GEDCOMTagType.CAUS)
            {
                custEvent.Cause = tagValue;
            }
            else if (tagType == GEDCOMTagType.TYPE)
            {
                custEvent.Classification = tagValue;
            }
            else if (tagType == GEDCOMTagType.DATE)
            {
                curTag = custEvent.Date;
                curTag.ParseString(tagValue);
            }
            else if (tagType == GEDCOMTagType.PLAC)
            {
                curTag = custEvent.Place;
                curTag.ParseString(tagValue);
                addHandler = AddPlaceTag;
            }
            else if (tagType == GEDCOMTagType.RELI)
            {
                custEvent.ReligiousAffilation = tagValue;
            }
            else if (tagType == GEDCOMTagType.RESN)
            {
                custEvent.Restriction = GEDCOMUtils.GetRestrictionVal(tagValue);
            }
            else if (tagType == GEDCOMTagType.PHON || tagType == GEDCOMTagType.EMAIL || tagType == GEDCOMTagType.FAX || tagType == GEDCOMTagType.WWW)
            {
                return AddAddressTag(custEvent.Address, tagLevel, tagId, tagValue);
            }
            else
            {
                return AddTagWithListsTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static StackTuple AddTaskRecordTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMTaskRecord taskRec = (GDMTaskRecord)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType._GOAL)
            {
                taskRec.Goal = tagValue;
            }
            else if (tagType == GEDCOMTagType._PRIORITY)
            {
                taskRec.Priority = GEDCOMUtils.GetPriorityVal(tagValue);
            }
            else if (tagType == GEDCOMTagType._STARTDATE)
            {
                curTag = taskRec.StartDate;
                curTag.ParseString(tagValue);
            }
            else if (tagType == GEDCOMTagType._STOPDATE)
            {
                curTag = taskRec.StopDate;
                curTag.ParseString(tagValue);
            }
            else
            {
                return AddRecordTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }
        private static StackTuple AddCommunicationRecordTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMCommunicationRecord commRec = (GDMCommunicationRecord)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.NAME)
            {
                commRec.CommName = tagValue;
            }
            else if (tagType == GEDCOMTagType.TYPE)
            {
                commRec.CommunicationType = GEDCOMUtils.GetCommunicationTypeVal(tagValue);
            }
            else if (tagType == GEDCOMTagType.FROM)
            {
                commRec.CommDirection = GDMCommunicationDir.cdFrom;
                commRec.Corresponder.ParseString(tagValue);
            }
            else if (tagType == GEDCOMTagType.TO)
            {
                commRec.CommDirection = GDMCommunicationDir.cdTo;
                commRec.Corresponder.ParseString(tagValue);
            }
            else if (tagType == GEDCOMTagType.DATE)
            {
                curTag = commRec.Date;
                curTag.ParseString(tagValue);
            }
            else
            {
                return AddRecordTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }
        private static StackTuple AddHeaderSourceTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMHeaderSource headerSource = (GDMHeaderSource)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.VERS)
            {
                headerSource.Version = tagValue;
            }
            else if (tagType == GEDCOMTagType.NAME)
            {
                headerSource.ProductName = tagValue;
            }
            else
            {
                return AddBaseTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }
        private static StackTuple AddSourceRecordTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMSourceRecord sourRec = (GDMSourceRecord)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.REPO)
            {
                curTag = sourRec.RepositoryCitations.Add(new GDMRepositoryCitation(sourRec));
                curTag.ParseString(tagValue);
            }
            else if (tagType == GEDCOMTagType.DATA)
            {
                curTag = sourRec.Data;
                addHandler = AddSourceDataTag;
            }
            else if (tagType == GEDCOMTagType.AUTH)
            {
                curTag = sourRec.Originator;
                curTag.ParseString(tagValue);
                addHandler = AddTextTag;
            }
            else if (tagType == GEDCOMTagType.PUBL)
            {
                curTag = sourRec.Publication;
                curTag.ParseString(tagValue);
                addHandler = AddTextTag;
            }
            else if (tagType == GEDCOMTagType.ABBR)
            {
                sourRec.ShortTitle = tagValue;
            }
            else if (tagType == GEDCOMTagType.TEXT)
            {
                curTag = sourRec.Text;
                curTag.ParseString(tagValue);
                addHandler = AddTextTag;
            }
            else if (tagType == GEDCOMTagType.TITL)
            {
                curTag = sourRec.Title;
                curTag.ParseString(tagValue);
                addHandler = AddTextTag;
            }
            else
            {
                return AddRecordTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }
        private static StackTuple AddResearchRecordTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMResearchRecord resRec = (GDMResearchRecord)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.NAME)
            {
                resRec.ResearchName = tagValue;
            }
            else if (tagType == GEDCOMTagType._PRIORITY)
            {
                resRec.Priority = GEDCOMUtils.GetPriorityVal(tagValue);
            }
            else if (tagType == GEDCOMTagType._STATUS)
            {
                resRec.Status = GEDCOMUtils.GetStatusVal(tagValue);
            }
            else if (tagType == GEDCOMTagType._PERCENT)
            {
                resRec.Percent = GEDCOMUtils.GetIntVal(tagValue);
            }
            else if (tagType == GEDCOMTagType._STARTDATE)
            {
                curTag = resRec.StartDate;
                curTag.ParseString(tagValue);
            }
            else if (tagType == GEDCOMTagType._STOPDATE)
            {
                curTag = resRec.StopDate;
                curTag.ParseString(tagValue);
            }
            else if (tagType == GEDCOMTagType._TASK)
            {
                curTag = resRec.Tasks.Add(new GDMPointer(resRec, tagId, tagValue));
            }
            else if (tagType == GEDCOMTagType._COMM)
            {
                curTag = resRec.Communications.Add(new GDMPointer(resRec, tagId, tagValue));
            }
            else if (tagType == GEDCOMTagType._GROUP)
            {
                curTag = resRec.Groups.Add(new GDMPointer(resRec, tagId, tagValue));
            }
            else
            {
                return AddRecordTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }
        private static StackTuple AddNoteRecordTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMNoteRecord noteRec = (GDMNoteRecord)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (IsTextTag(tagType))
            {
                return AddTextTag(noteRec, tagLevel, tagId, tagValue);
            }
            else
            {
                return AddRecordTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }
        private static StackTuple AddRepositoryRecordTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMRepositoryRecord repoRec = (GDMRepositoryRecord)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.NAME)
            {
                repoRec.RepositoryName = tagValue;
            }
            else if (tagType == GEDCOMTagType.ADDR)
            {
                curTag = repoRec.Address;
                curTag.ParseString(tagValue);
                addHandler = AddAddressTag;
            }
            else if (tagType == GEDCOMTagType.PHON || tagType == GEDCOMTagType.EMAIL || tagType == GEDCOMTagType.FAX || tagType == GEDCOMTagType.WWW)
            {
                return AddAddressTag(repoRec.Address, tagLevel, tagId, tagValue);
            }
            else
            {
                return AddRecordTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }
        private static StackTuple AddGroupRecordTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMGroupRecord groupRec = (GDMGroupRecord)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.NAME)
            {
                groupRec.GroupName = tagValue;
            }
            else if (tagType == GEDCOMTagType._MEMBER)
            {
                curTag = groupRec.Members.Add(new GDMIndividualLink(groupRec, tagId, tagValue));
            }
            else
            {
                return AddRecordTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }
        private static StackTuple AddMultimediaRecordTag(GDMObject owner, int tagLevel, int tagId, string tagValue)
        {
            GDMMultimediaRecord mmRec = (GDMMultimediaRecord)owner;
            GDMTag curTag = null;
            AddTagHandler addHandler = null;

            GEDCOMTagType tagType = (GEDCOMTagType)tagId;
            if (tagType == GEDCOMTagType.FILE)
            {
                curTag = mmRec.FileReferences.Add(new GDMFileReferenceWithTitle(mmRec));
                curTag.ParseString(tagValue);
                addHandler = AddFileReferenceWithTitleTag;
            }
            else
            {
                return AddRecordTag(owner, tagLevel, tagId, tagValue);
            }

            return CreateReaderStackTuple(tagLevel, curTag, addHandler);
        }

        private static bool IsTextTag(string tagName)
        {
            return (tagName == GEDCOMTagName.CONT || tagName == GEDCOMTagName.CONC);
        }

        private static bool IsTextTag(GEDCOMTagType tag)
        {
            return (tag == GEDCOMTagType.CONT || tag == GEDCOMTagType.CONC);
        }

        public static AddTagHandler GetAddHandler(int tagId)
        {
            GEDCOMTagProps tagInfo = GEDCOMTagsTable.GetTagProps(tagId);
            return (tagInfo != null) ? tagInfo.AddHandler : AddBaseTag;
        }
    }
}
