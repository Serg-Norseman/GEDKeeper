/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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
using GDModel;
using GKCore;
using GKCore.Design.Graphics;
using GKCore.Export;
using GKCore.Interfaces;

namespace GKStdReports
{
    /// <summary>
    /// This report prints the number of ancestors found, coverage factor,
    /// implex factor and coefficient of consanguinity.
    /// </summary>
    public class AncestorStatisticsReport : ReportExporterEx
    {
        private readonly GDMIndividualRecord fPerson;
        private IFont fChaptFont;
        private IFont fTextFont;
        private IFont fTitleFont;

        public AncestorStatisticsReport(IBaseWindow baseWin, GDMIndividualRecord selectedPerson)
            : base(baseWin, false)
        {
            fTitle = SRLangMan.LS(PLS.AncestorStatisticsReport);
            fPerson = selectedPerson;
        }

        protected override void InternalGenerate()
        {
            IColor clrBlack = AppHost.GfxProvider.CreateColor(0x000000);
            fTitleFont = fWriter.CreateFont("", 22f, true, false, clrBlack);
            fChaptFont = fWriter.CreateFont("", 14f, true, false, clrBlack);
            fTextFont = fWriter.CreateFont("", 10f, false, false, clrBlack);

            ClearStats();
            ComputeImplexFactor(fPerson);
            ComputeConsanguinityFactor(fPerson);

            PrintHeader();
            PrintImplexStats();
            PrintCommonAncestors();
            PrintConsanguinityStats();
        }

        private void WriteLine(string text)
        {
            fWriter.AddParagraph(text, fTextFont, TextAlignment.taLeft);
        }

        private void WriteLine(PLS lsid, params object[] args)
        {
            fWriter.AddParagraph(Localize(lsid, args), fTextFont, TextAlignment.taLeft);
        }

        private void AddCell(string content, TextAlignment alignment = TextAlignment.taLeft)
        {
            fWriter.AddTableCell(content, fTextFont, alignment);
        }

        private class GenerationInfo
        {
            public int Level;

            public int PossibleCount;
            public int KnownCount;
            public int DiffCount;

            public int PossibleCumul;
            public int KnownCumul;
            public int DiffCumul;

            public double Coverage;
            public double CoverageCumul;
            public double Implex;

            public GenerationInfo(int level)
            {
                Level = level;
            }
        }

        private class ConsanguinityInfo
        {
            public GDMIndividualRecord Indi;
            public int Count;
            public double ConsanguinityFactor;
            public Stack<string> StackIndi = new Stack<string>();

            public ConsanguinityInfo(GDMIndividualRecord indi)
            {
                Indi = indi;
            }
        }

        private readonly Dictionary<string, ConsanguinityInfo> fConsanguinityCommonIndiMap = new Dictionary<string, ConsanguinityInfo>();
        private double fConsanguinityFactor;
        private readonly List<GenerationInfo> fGenerations = new List<GenerationInfo>();
        private readonly Dictionary<string, GDMIndividualRecord> fImplexCommonIndiMap = new Dictionary<string, GDMIndividualRecord>();
        private double fImplexFactor;
        private readonly HashSet<string> fIndiSet = new HashSet<string>();
        private readonly HashSet<string> fCommonAncestorSet = new HashSet<string>();

        private void ClearStats()
        {
            // Clear implex statistics
            fImplexFactor = 0;
            fGenerations.Clear();
            fIndiSet.Clear();
            fCommonAncestorSet.Clear();
            fImplexCommonIndiMap.Clear();

            // Clear consanguinity statistics
            fConsanguinityFactor = 0;
            fConsanguinityCommonIndiMap.Clear();
        }

        private void PrintHeader()
        {
            fWriter.AddParagraph(Localize(PLS.AncestorStatisticsReport), fTitleFont);
            fWriter.NewLine();

            WriteLine(PLS.RootIndividual, GetName(fPerson));
            WriteLine(PLS.ImplexFactor, fImplexFactor);
            WriteLine(PLS.ConsanguinityFactor, fConsanguinityFactor);
            fWriter.NewLine();
        }

        private void PrintImplexStats()
        {
            int tblSize = fGenerations.Count;
            fWriter.BeginTable(8, tblSize + 1);

            // Print header
            fWriter.BeginTableRow();
            AddCell(Localize(PLS.Generation), TextAlignment.taLeft);
            AddCell(Localize(PLS.Possible), TextAlignment.taRight);
            AddCell(Localize(PLS.Known), TextAlignment.taRight);
            AddCell(Localize(PLS.KnownPercent), TextAlignment.taRight);
            AddCell(Localize(PLS.Cumul), TextAlignment.taRight);
            AddCell(Localize(PLS.CumulPercent), TextAlignment.taRight);
            AddCell(Localize(PLS.Diff), TextAlignment.taRight);
            AddCell(Localize(PLS.Implex), TextAlignment.taRight);
            fWriter.EndTableRow();

            // Iteration on generations
            foreach (var info in fGenerations) {
                fWriter.BeginTableRow();
                AddCell("" + info.Level, TextAlignment.taLeft);
                AddCell("" + info.PossibleCount, TextAlignment.taRight);
                AddCell("" + info.KnownCount, TextAlignment.taRight);
                AddCell("" + info.Coverage + "%", TextAlignment.taRight);
                AddCell("" + info.KnownCumul, TextAlignment.taRight);
                AddCell("" + info.CoverageCumul + "%", TextAlignment.taRight);
                AddCell("" + info.DiffCount, TextAlignment.taRight);
                AddCell("" + info.Implex + "%", TextAlignment.taRight);
                fWriter.EndTableRow();
            }

            fWriter.EndTable();

            fWriter.NewLine();
        }

        private void PrintCommonAncestors()
        {
            if (fImplexCommonIndiMap.Count == 0) return;

            // Print table of common individuals
            fWriter.AddParagraph(Localize(PLS.CommonAncestors), fChaptFont);
            foreach (var itr in fImplexCommonIndiMap.Values) {
                WriteLine(GetName(itr));
            }
            fWriter.NewLine();
        }

        private void PrintConsanguinityStats()
        {
            int tblSize = fConsanguinityCommonIndiMap.Count;
            if (tblSize == 0) return;

            WriteLine(PLS.ConsanguinityCommonAncestors);

            fWriter.BeginTable(2, tblSize);
            foreach (var info in fConsanguinityCommonIndiMap.Values) {
                fWriter.BeginTableRow();
                AddCell(GetName(info.Indi), TextAlignment.taLeft);
                AddCell("" + info.ConsanguinityFactor + "%", TextAlignment.taRight);
                fWriter.EndTableRow();
            }
            fWriter.EndTable();
        }

        private void ComputeImplexFactor(GDMIndividualRecord indi)
        {
            // Initialize the first generation with the selected individual
            var listIndi = new List<GDMIndividualRecord>();
            listIndi.Add(indi);

            // Compute statistics one generation after the other
            int level = 1;
            while (listIndi.Count != 0) {
                var listParent = new List<GDMIndividualRecord>();
                ComputeGeneration(level, listIndi, listParent);
                listIndi = listParent;
                level++;
            }

            // Compute cumul statistics
            int possibleCumul = 0;
            int knownCumul = 0;
            int diffCumul = 0;
            foreach (var info in fGenerations) {
                // Compute possible
                info.PossibleCount = (int)Math.Pow(2.0f, info.Level - 1);

                // Compute cumuls
                possibleCumul += info.PossibleCount;
                knownCumul += info.KnownCount;
                diffCumul += info.DiffCount;

                // Store cumuls
                info.PossibleCumul = possibleCumul;
                info.KnownCumul = knownCumul;
                info.DiffCumul = diffCumul;

                // Compute coverage
                info.Coverage = (10000 * info.KnownCount / info.PossibleCount) / 100d;
                info.CoverageCumul = (10000 * info.KnownCumul / info.PossibleCumul) / 100d;

                // Compute implex
                if (knownCumul != 0) {
                    info.Implex = (10000 * (info.KnownCumul - info.DiffCumul) / info.KnownCumul) / 100d;
                    fImplexFactor = info.Implex;
                }
            }
        }

        /// <summary>
        /// Add an individual and all its ancestors in the common ancestor list.
        /// </summary>
        /// <param name="indi">Common ancestor.</param>
        private void AddCommonAncestor(GDMIndividualRecord indi)
        {
            if (indi == null) return;

            // Add individual to the list
            fCommonAncestorSet.Add(indi.XRef);

            // Add parents to the list
            var famRec = fTree.GetParentsFamily(indi);
            if (famRec != null) {
                AddCommonAncestor(fTree.GetPtrValue(famRec.Wife));
                AddCommonAncestor(fTree.GetPtrValue(famRec.Husband));
            }
        }

        /// <summary>
        /// Computes statistics for the specified generation.
        /// </summary>
        /// <param name="level">Current generation level.</param>
        /// <param name="listIndi">Individuals of a generation.</param>
        /// <param name="listParent">Individuals of the next generation.</param>
        private void ComputeGeneration(int level, List<GDMIndividualRecord> listIndi, List<GDMIndividualRecord> listParent)
        {
            // Prepare generation information
            GenerationInfo info = new GenerationInfo(level);
            fGenerations.Add(info);

            // Scan individual of the generation
            foreach (var indi in listIndi) {
                // Get ancestor XRef and search it in the list
                var iXRef = indi.XRef;
                if (fIndiSet.Contains(iXRef)) {
                    // Check if this indivual has already been listed
                    if (!fCommonAncestorSet.Contains(iXRef)) {
                        // Print individual description
                        fImplexCommonIndiMap[iXRef] = indi;

                        // Add individual and its ancestors to the list
                        AddCommonAncestor(indi);
                    }
                } else {
                    // This is a new ancestor
                    fIndiSet.Add(iXRef);
                    info.DiffCount++;
                }

                // Count this ancestor in all case
                info.KnownCount++;

                // Get parents
                GDMIndividualRecord father, mother;
                fTree.GetParents(indi, out father, out mother);

                if (father != null)
                    listParent.Add(father);

                if (mother != null)
                    listParent.Add(mother);
            }
        }

        private void ComputeConsanguinityFactor(GDMIndividualRecord indi)
        {
            fConsanguinityFactor = 0;

            var famRec = fTree.GetParentsFamily(indi);
            if (famRec == null) return;

            var stackRight = new Stack<string>();
            var stackLeft = new Stack<string>();
            CheckRightTree(fTree.GetPtrValue(famRec.Wife), 0, stackRight, fTree.GetPtrValue(famRec.Husband), 0, stackLeft);
        }

        /// <summary>
        /// Check the ancestors of one parent to compute the consanguinity factor.
        /// </summary>
        /// <param name="indiRight">Current individual.</param>
        /// <param name="levelRight">Current generation level.</param>
        /// <param name="stackRight">Current ancestor list.</param>
        /// <param name="indiLeft">Current individual of other the tree.</param>
        /// <param name="levelLeft">Current generation level of other the tree.</param>
        /// <param name="stackLeft">Current ancestor list of other the tree.</param>
        private void CheckRightTree(GDMIndividualRecord indiRight, int levelRight, Stack<string> stackRight,
            GDMIndividualRecord indiLeft, int levelLeft, Stack<string> stackLeft)
        {
            // Exit if an individual is missing
            if (indiRight == null || indiLeft == null) return;

            // There is consanguinity only if an individual appears in father and mother tree.
            // Search if this individual appears in the other tree
            SearchInLeftTree(indiRight, levelRight, stackRight, indiLeft, 0, stackLeft);

            // Add individual XRef to ancestor stack
            var rightXRef = indiRight.XRef;
            stackRight.Push(rightXRef);

            // If no family, nothing to do
            var famRec = fTree.GetParentsFamily(indiRight);
            if (famRec != null) {
                // Continue to check the tree
                // Recursive call to mother and father
                CheckRightTree(fTree.GetPtrValue(famRec.Wife), levelRight + 1, stackRight, indiLeft, levelLeft, stackLeft);
                CheckRightTree(fTree.GetPtrValue(famRec.Husband), levelRight + 1, stackRight, indiLeft, levelLeft, stackLeft);
            }

            // Remove individual XRef from ancestor stack
            stackRight.Pop();
        }

        /// <summary>
        /// Search reference individual in the the ancestors of second parent.
        /// </summary>
        /// <param name="indiRight">Reference individual.</param>
        /// <param name="levelRight">Reference generation level.</param>
        /// <param name="stackRight">Reference ancestor.</param>
        /// <param name="indiLeft">Current individual.</param>
        /// <param name="levelLeft">Current generation level.</param>
        /// <param name="stackLeft">Current ancestor list.</param>
        private void SearchInLeftTree(GDMIndividualRecord indiRight, int levelRight, Stack<string> stackRight,
            GDMIndividualRecord indiLeft, int levelLeft, Stack<string> stackLeft)
        {
            // Exit if an individual is missing
            if (indiRight == null || indiLeft == null) return;

            // Do not check further if this individual is in the ancestor stack
            // on the other tree.
            var leftXRef = indiLeft.XRef;
            if (stackRight.Contains(leftXRef)) return;

            // Get XRef of the reference individual and check if the current individual
            // is the same.
            var rightXRef = indiRight.XRef;
            if (rightXRef == leftXRef) {
                // Check if indivividual is already in list
                ConsanguinityInfo info;
                fConsanguinityCommonIndiMap.TryGetValue(rightXRef, out info);
                if (info == null) {
                    // Create info about individual
                    info = new ConsanguinityInfo(indiRight);
                }

                // Add this common person in list
                fConsanguinityCommonIndiMap[rightXRef] = info;

                // Compute coefficient of consanguinity for this case
                double power = levelRight + levelLeft + 1;
                double consanguinityPart = Math.Pow(0.5, power);
                fConsanguinityFactor += consanguinityPart;
                info.ConsanguinityFactor += consanguinityPart;
                info.Count++;
                return;
            }

            // Add individual XRef to ancestor stack
            stackLeft.Push(leftXRef);

            // If no family, nothing to do
            var famRec = fTree.GetParentsFamily(indiLeft);
            if (famRec != null) {
                // Recursive call to mother and father
                SearchInLeftTree(indiRight, levelRight, stackRight, fTree.GetPtrValue(famRec.Wife), levelLeft + 1, stackLeft);
                SearchInLeftTree(indiRight, levelRight, stackRight, fTree.GetPtrValue(famRec.Husband), levelLeft + 1, stackLeft);
            }

            // Remove individual XRef from ancestor stack
            stackLeft.Pop();
        }
    }
}
