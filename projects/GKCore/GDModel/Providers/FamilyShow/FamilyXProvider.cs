/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
using System.Collections.Generic;
using System.IO;
using System.IO.Packaging;
using System.Linq;
using System.Xml;
using GDModel.Providers.GEDCOM;
using GKCore;

namespace GDModel.Providers.FamilyShow
{

    /// <summary>
    /// Processing the FamilyX format is one part of the Genealogical Data Model (GDM).
    /// </summary>
    public class FamilyXProvider : FileProvider
    {
        private const string OPCContentFileName = "content.xml";


        static FamilyXProvider()
        {
            // Static initialization of the GEDCOMProvider is needed, 
            // otherwise the standard tag identifiers are out of sync
            SysUtils.DoNotInline(GEDCOMProvider.GEDCOMFormats);
        }

        public FamilyXProvider(GDMTree tree) : base(tree)
        {
        }

        public override string GetFilesFilter()
        {
            return "Family.Show files (*.familyx)|*.familyx";
        }

        public override void LoadFromStreamExt(Stream fileStream, Stream inputStream, bool charsetDetection = false)
        {
            using (Package package = Package.Open(inputStream, FileMode.Open, FileAccess.Read)) {
                PackagePart documentPart = package.GetPart(new Uri(@"/" + OPCContentFileName, UriKind.Relative));
                var docPartStream = documentPart.GetStream();
                //using (MemoryStream memStream = new MemoryStream()) {
                //OPCUtility.CopyStream(docPartStream, memStream);
                //memStream.Position = 0;
                ReadStream(inputStream, docPartStream, charsetDetection);
                //}
            }
        }

        private enum FXTag
        {
            Unknown,
            Gender,
            Restriction,
            FirstName,
            LastName,
            Suffix,
            IsLiving,
            BirthDate,
            BirthPlace,
            DeathDate,
            DeathPlace,
            Note,
            Relationship,
            SpouseLink,
            ChildLink
        }

        private enum RelationshipType { None, Spouse, Child }

        protected override void ReadStream(Stream fileStream, Stream inputStream, bool charsetDetection = false)
        {
            fTree.State = GDMTreeState.osLoading;
            try {
                var progressCallback = fTree.ProgressCallback;

                long fileSize = fileStream.Length;
                int progress = 0;

                GDMIndividualRecord lastIndividual = null;
                FXTag lastTagType = FXTag.Unknown;
                RelationshipType relationshipType = RelationshipType.None;
                var families = new List<FamilyRec>();
                var children = new List<ChildRec>();
                var indiIdents = new Dictionary<string, GDMIndividualRecord>();

                XmlReaderSettings settings = new XmlReaderSettings();
                settings.DtdProcessing = DtdProcessing.Ignore;
                using (XmlReader xr = XmlReader.Create(inputStream, settings)) {
                    while (xr.Read()) {
                        if (xr.NodeType == XmlNodeType.Element && !xr.IsEmptyElement) {
                            string nodeType = xr.Name;
                            if (nodeType == "Person") {
                                lastIndividual = fTree.CreateIndividual();
                                var persName = new GDMPersonalName();
                                lastIndividual.AddPersonalName(persName);

                                // add an empty birth event
                                AddEvent(lastIndividual, GEDCOMTagName.BIRT);

                                lastIndividual.UID = xr.GetAttribute("Id");
                                indiIdents.Add(lastIndividual.UID, lastIndividual);
                            } else if (nodeType == "Gender") {
                                lastTagType = FXTag.Gender;
                            } else if (nodeType == "Restriction") {
                                lastTagType = FXTag.Restriction;
                            } else if (nodeType == "FirstName") {
                                lastTagType = FXTag.FirstName;
                            } else if (nodeType == "LastName") {
                                lastTagType = FXTag.LastName;
                            } else if (nodeType == "Suffix") {
                                lastTagType = FXTag.Suffix;
                            } else if (nodeType == "IsLiving") {
                                lastTagType = FXTag.IsLiving;
                            } else if (nodeType == "Note") {
                                lastTagType = FXTag.Note;
                            } else if (nodeType == "BirthDate") {
                                lastTagType = FXTag.BirthDate;
                            } else if (nodeType == "BirthPlace") {
                                lastTagType = FXTag.BirthPlace;
                            } else if (nodeType == "DeathDate") {
                                lastTagType = FXTag.DeathDate;
                            } else if (nodeType == "DeathPlace") {
                                lastTagType = FXTag.DeathPlace;
                            } else if (nodeType == "RelationshipType") {
                                lastTagType = FXTag.Relationship;
                            } else if (nodeType == "PersonId") {
                                switch (relationshipType) {
                                    case RelationshipType.Spouse:
                                        lastTagType = FXTag.SpouseLink;
                                        break;
                                    case RelationshipType.Child:
                                        lastTagType = FXTag.ChildLink;
                                        break;
                                    default:
                                        lastTagType = FXTag.Unknown;
                                        break;
                                }
                                relationshipType = RelationshipType.None;
                            }
                        } else if (xr.NodeType == XmlNodeType.Text) {
                            string nodeValue = xr.Value;
                            if (!string.IsNullOrEmpty(nodeValue) && (lastIndividual != null)) {
                                switch (lastTagType) {
                                    case FXTag.Gender:
                                        if (nodeValue == "Male") {
                                            lastIndividual.Sex = GDMSex.svMale;
                                        } else if (nodeValue == "Female") {
                                            lastIndividual.Sex = GDMSex.svFemale;
                                        }
                                        break;

                                    case FXTag.Restriction:
                                        // FIXME: parse from nodeValue
                                        lastIndividual.Restriction = GDMRestriction.rnNone;
                                        break;

                                    case FXTag.FirstName:
                                        lastIndividual.PersonalNames[0].Given = nodeValue;
                                        break;

                                    case FXTag.LastName:
                                        lastIndividual.PersonalNames[0].Surname = nodeValue;
                                        break;

                                    case FXTag.Suffix:
                                        lastIndividual.PersonalNames[0].NameSuffix = nodeValue;
                                        break;

                                    case FXTag.IsLiving:
                                        bool isLiving = string.Equals(nodeValue, "true", StringComparison.InvariantCultureIgnoreCase);
                                        if (!isLiving) AddEvent(lastIndividual, GEDCOMTagName.DEAT);
                                        break;

                                    case FXTag.BirthDate:
                                    case FXTag.BirthPlace:
                                        var birthEvent = lastIndividual.FindEvent("BIRT");
                                        if (lastTagType == FXTag.BirthDate) {
                                            SetEventDate(birthEvent, nodeValue);
                                        } else {
                                            birthEvent.Place.StringValue = nodeValue;
                                        }
                                        break;

                                    case FXTag.DeathDate:
                                    case FXTag.DeathPlace:
                                        var deathEvent = lastIndividual.FindEvent("DEAT");
                                        // IsLiving may be wrong
                                        if (deathEvent == null) {
                                            deathEvent = AddEvent(lastIndividual, GEDCOMTagName.DEAT);
                                        }
                                        if (lastTagType == FXTag.DeathDate) {
                                            SetEventDate(deathEvent, nodeValue);
                                        } else {
                                            deathEvent.Place.StringValue = nodeValue;
                                        }
                                        break;

                                    case FXTag.Note:
                                        AddNote(lastIndividual, nodeValue);
                                        break;

                                    case FXTag.Relationship:
                                        if (nodeValue == "Spouse") {
                                            relationshipType = RelationshipType.Spouse;
                                        } else if (nodeValue == "Child") {
                                            relationshipType = RelationshipType.Child;
                                        }
                                        break;

                                    case FXTag.SpouseLink:
                                        ProcessSpouse(families, lastIndividual, nodeValue);
                                        break;

                                    case FXTag.ChildLink:
                                        ProcessChild(children, lastIndividual, nodeValue);
                                        break;
                                }
                                lastTagType = FXTag.Unknown;
                            }
                        }

                        if (progressCallback != null) {
                            int newProgress = (int)Math.Min(100, (fileStream.Position * 100.0f) / fileSize);
                            if (progress != newProgress) {
                                progress = newProgress;
                                progressCallback.StepTo(progress);
                            }
                        }
                    }
                }

                foreach (var fam in families) {
                    GDMIndividualRecord husbRec;
                    indiIdents.TryGetValue(fam.HusbandId, out husbRec);

                    GDMIndividualRecord wifeRec;
                    indiIdents.TryGetValue(fam.WifeId, out wifeRec);

                    GDMFamilyRecord famRec = fTree.CreateFamily();
                    famRec.AddSpouse(husbRec);
                    famRec.AddSpouse(wifeRec);
                }

                foreach (var child in children) {
                    GDMIndividualRecord fathRec;
                    indiIdents.TryGetValue(child.FatherId, out fathRec);

                    GDMIndividualRecord mothRec;
                    indiIdents.TryGetValue(child.MotherId, out mothRec);

                    GDMIndividualRecord childRec;
                    indiIdents.TryGetValue(child.ChildId, out childRec);

                    GDMFamilyRecord famRec = GetParentsFamily(fathRec, mothRec);
                    famRec.AddChild(childRec);
                }
            } finally {
                fTree.State = GDMTreeState.osReady;
            }
        }

        private GDMFamilyRecord GetParentsFamily(GDMIndividualRecord father, GDMIndividualRecord mother)
        {
            GDMFamilyRecord result = null;

            string fatherXRef = (father == null) ? string.Empty : father.XRef;
            string motherXRef = (mother == null) ? string.Empty : mother.XRef;

            var famEnum = fTree.GetEnumerator<GDMFamilyRecord>();
            GDMFamilyRecord famRec;
            while (famEnum.MoveNext(out famRec)) {
                if (famRec.Husband.XRef == fatherXRef && famRec.Wife.XRef == motherXRef) {
                    result = famRec;
                    break;
                }
            }

            if (result == null) {
                result = fTree.CreateFamily();
                result.AddSpouse(father);
                result.AddSpouse(mother);
            }

            return result;
        }

        private void ProcessSpouse(ICollection<FamilyRec> families, GDMIndividualRecord individual, string spouseId)
        {
            string husbId, wifeId;
            if (individual.Sex == GDMSex.svMale) {
                husbId = individual.UID;
                wifeId = spouseId;
            } else {
                husbId = spouseId;
                wifeId = individual.UID;
            }

            var famRec = families.SingleOrDefault(f => (f.HusbandId == husbId && f.WifeId == wifeId));
            if (famRec == null) {
                families.Add(new FamilyRec(husbId, wifeId));
            }
        }

        private void ProcessChild(ICollection<ChildRec> children, GDMIndividualRecord individual, string childId)
        {
            var childRec = children.SingleOrDefault(f => (f.ChildId == childId));
            if (childRec == null) {
                childRec = new ChildRec(childId);
                children.Add(childRec);
            }

            if (individual.Sex == GDMSex.svMale) {
                childRec.FatherId = individual.UID;
            } else {
                childRec.MotherId = individual.UID;
            }
        }

        private void SetEventDate(GDMCustomEvent evt, string dateValue)
        {
            try {
                evt.Date.SetDateTime(DateTime.ParseExact(dateValue, "yyyy-MM-ddTHH:mm:ss", null));
            } catch (Exception ex) {
                Logger.WriteError("FamilyXProvider.SetEventDate(" + dateValue + ")", ex);
            }
        }

        private GDMCustomEvent AddEvent(GDMRecordWithEvents indiRec, string eventName)
        {
            GDMCustomEvent result = new GDMIndividualEvent();
            result.SetName(eventName);
            indiRec.AddEvent(result);
            return result;
        }

        private void AddNote(GDMRecord indiRec, string noteText)
        {
            var noteRec = fTree.CreateNote();
            noteRec.Lines.Text = noteText;
            var notes = new GDMNotes();
            notes.XRef = noteRec.XRef;
            indiRec.Notes.Add(notes);
        }

        private class FamilyRec
        {
            public string HusbandId;
            public string WifeId;

            public FamilyRec(string husbId, string wifeId)
            {
                HusbandId = husbId;
                WifeId = wifeId;
            }
        }

        private class ChildRec
        {
            public string FatherId;
            public string MotherId;
            public string ChildId;

            public ChildRec(string childId)
            {
                ChildId = childId;
            }
        }
    }
}
