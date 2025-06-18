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

//#define DESK_METHOD

using System;
using GDModel;

namespace GKCore.Charts
{
    public class DefTreeLayout : ITreeLayout
    {
        private const int EDGES_SIZE = 255; // -127..+127, 0..254, 255, 1 unused

#if !DESK_METHOD
        private int[] fEdges;
#else
        private TreeChartDesk fDesk;
#endif
        private TreeChartModel fModel;


        private int fBranchDistance; // distance between spouses in descendants
        private bool fExtendedTree;
        private bool fInvertedTree;
        private int fLevelDistance; // distance between generations
        private int fMargins;
        private bool fMinimizingWidth;
        private TreeChartPerson fRoot;
        private int fSpouseDistance; // distance between spouses in ancestors


        public DefTreeLayout()
        {
#if !DESK_METHOD
            fEdges = new int[EDGES_SIZE];
#else
            fDesk = new TreeChartDesk();
#endif
        }

        public void SetModel(ChartModel model)
        {
            fModel = (TreeChartModel)model;
        }

        public void RecalcChart()
        {
            //ClearEdges();

            fRoot = fModel.Root;
            fMargins = fModel.Margins;
            fBranchDistance = fModel.BranchDistance;
            fExtendedTree = fModel.IsExtendedTree();
            fInvertedTree = fModel.Options.InvertedTree;
            fLevelDistance = fModel.LevelDistance;
            fMinimizingWidth = fModel.Options.MinimizingWidth;
            fSpouseDistance = fModel.SpouseDistance;

            switch (fModel.Kind) {
                case TreeChartKind.ckAncestors:
                    RecalcAncestorsChart();
                    break;

                case TreeChartKind.ckDescendants:
                    RecalcDescendantsChart(true);
                    break;

                case TreeChartKind.ckBoth:
                    RecalcAncestorsChart();
                    RecalcDescendantsChart(false);
                    break;
            }
        }

        #region Common

#if !DESK_METHOD
        private void ClearEdges()
        {
            Array.Clear(fEdges, 0, EDGES_SIZE);
        }
#endif

        private void SetEdge(int generation, int value)
        {
#if !DESK_METHOD
            if (generation < -127 || generation > 127)
                throw new ArgumentOutOfRangeException("generation");

            int index = generation + 127;
            fEdges[index] = value;
#endif
        }

        private void SetEdge(TreeChartPerson person)
        {
            SetEdge(person.Generation, person.Rect.Right);
        }

        /// <summary>
        /// If the option to minimize the diagram width is not selected,
        /// the function takes the right edge from the previous branch of the given generation,
        /// otherwise it searches for the rightmost edge from the current generation and all subsequent ones.
        /// </summary>
        private int GetEdge(TreeChartPerson person, bool ancestors)
        {
#if !DESK_METHOD
            int generation = person.Generation;

            if (generation < -127 || generation > 127)
                throw new ArgumentOutOfRangeException("generation");

            int index = generation + 127;
            int result = fEdges[index];

            if (result > 0 && !fMinimizingWidth) {
                if (ancestors) {
                    for (int i = index - 1; i >= 0; i--) {
                        int edge = fEdges[i];
                        if (edge == 0) break;
                        result = Math.Max(result, edge);
                    }
                } else {
                    for (int i = index + 1; i < EDGES_SIZE; i++) {
                        int edge = fEdges[i];
                        if (edge == 0) break;
                        result = Math.Max(result, edge);
                    }
                }
            }

            return result;
#else
            var left = fDesk.GetBefore(person);

            if (left == null) {
                return 0;
            } else {
                return left.Rect.Right;
            }
#endif
        }

        private int GetCurrentBranchLeftBound(TreeChartPerson person, bool ancestors)
        {
            int branchDist = (ancestors) ? fBranchDistance : GetBranchDistance(person);
            int edge = GetEdge(person, ancestors);
            int offset = (edge > 0) ? branchDist : fMargins;
            int bound = edge + offset;
            return bound;
        }

        private int GetBranchDistance(TreeChartPerson person)
        {
            int mdWidth = person.MarriageDateWidth;
            return (mdWidth == 0) ? fBranchDistance : Math.Max(fBranchDistance, mdWidth + fModel.NodePadding * 2);
        }

        /// <summary>
        /// The function returns the Y-coordinate of the next generation,
        /// depending on the type of movement (ancestors, descendants) and the chart inversion option.
        /// </summary>
        private int NextGenY(TreeChartPerson person, bool ancestors)
        {
            int sign = (ancestors) ? -1 : +1;
            int sign2 = (!fInvertedTree) ? +1 : -1;
            int offset = (fLevelDistance + person.Height) * sign * sign2;
            return person.PtY + offset;
        }

        #endregion

        #region Ancestors

        /// <summary>
        /// A recursive shift of previously passed generations of descendants is needed in this case:
        /// if the previously passed generation did not intersect on the left, and on the current generation
        /// the left branch hangs over the previous ones, only the current generation will be broken to the right,
        /// and the underlying ones will remain under the overhanging left branch.
        /// </summary>
        private void ShiftAnc(TreeChartPerson person, int offset)
        {
            TreeChartPerson pp = person;
            while (pp != null && pp.Generation <= 0) {
                pp.PtX += offset;
                SetEdge(pp);
                pp = (pp.GetChildsCount() < 1) ? null : pp.GetChild(0);
            }
        }

        private void RecalcAnc(TreeChartPerson person, int ptX, int ptY)
        {
            if (person == null) return;

            person.SetPt(ptX, ptY);
            int bound = GetCurrentBranchLeftBound(person, true);
            if (person.Rect.Left < bound) {
                ShiftAnc(person, bound - person.Rect.Left);
            }
            SetEdge(person);

            person.IsVisible = true;
            if (person.IsCollapsed) {
                return;
            }

            int nextY = NextGenY(person, true);
            if (person.Father != null && person.Mother != null) {
                RecalcAnc(person.Father, person.PtX - (fSpouseDistance + person.Father.Width / 2), nextY);
                RecalcAnc(person.Mother, person.PtX + (fSpouseDistance + person.Mother.Width / 2), nextY);

                // alignment of child coordinates between parents
                person.PtX = (person.Father.PtX + person.Mother.PtX) / 2;
                SetEdge(person);
            } else {
                TreeChartPerson parent = null;
                if (person.Father != null) {
                    parent = person.Father;
                } else if (person.Mother != null) {
                    parent = person.Mother;
                }

                if (parent != null) {
                    RecalcAnc(parent, person.PtX, nextY);
                }
            }
        }

        private void RecalcAncestorsChart()
        {
#if !DESK_METHOD
            ClearEdges();
#endif

            if (fExtendedTree) {
                // the man's spouses align to his right
                if (fRoot.Sex == GDMSex.svMale) {
                    RecalcAnc(fRoot, fMargins, fMargins);
                }

                int spousesCount = fRoot.GetSpousesCount();
                for (int i = 0; i < spousesCount; i++) {
                    TreeChartPerson sp = fRoot.GetSpouse(i);
                    RecalcAnc(sp, fMargins, fMargins);
                }

                // the woman's spouses align to her left
                if (fRoot.Sex == GDMSex.svFemale) {
                    RecalcAnc(fRoot, fMargins, fMargins);
                }
            } else {
                RecalcAnc(fRoot, fMargins, fMargins);
            }
        }

        #endregion

        #region Descendants

        /// <summary>
        /// Shift spouse and central person (if female) if needed to shift their descendants.
        /// </summary>
        /// <param name="person"></param>
        /// <param name="offset"></param>
        /// <returns>
        ///     if true, then the shift of the previous iteration can be performed
        /// </returns>
        private bool ShiftSpousesFrom(TreeChartPerson person, int offset)
        {
            // can not move the ancestors of the spouses to the left,
            // because there may be ancestors of the central person or another spouse;
            // it is impossible to find out exactly where the other branch is on the left,
            // because at this stage, we do not know which branch is being processed
            if (offset < 0)
                return false;

            TreeChartPerson basePerson, lastPers = null;
            int startIndex;

            if (person.BaseSpouse == null) {
                // root
                basePerson = person;
                startIndex = (person.Sex == GDMSex.svMale) ? 0 : basePerson.GetSpousesCount();
            } else {
                // root spouse
                basePerson = person.BaseSpouse;
                startIndex = basePerson.IndexOfSpouse(person);

                // only a woman can be on the right as a central person,
                // so if we move her spouse, we need to move her
                if (person.Sex == GDMSex.svMale) {
                    lastPers = basePerson;
                }

                // if the base spouse of the current person is a man
                // and there is only one marriage, we also shift his
                if (person.BaseSpouse.Sex == GDMSex.svMale && person.BaseSpouse.GetSpousesCount() == 1) {
                    lastPers = basePerson;
                }
            }

            for (int i = startIndex; i < basePerson.GetSpousesCount(); i++) {
                var sp = basePerson.GetSpouse(i);
                if (!ShiftPerson(sp, offset, false)) return false;
            }

            if (lastPers != null) {
                if (!ShiftPerson(lastPers, offset, false)) return false;
            }

            return true;
        }

        /// <summary>
        /// Recursive shift of all higher ancestors (straight line).
        /// </summary>
        /// <param name="person"></param>
        /// <param name="offset"></param>
        /// <param name="ext">
        ///     If true - use additional checks.
        /// </param>
        /// <param name="verify">
        ///     If true - check for violation of the boundary of the left branch,
        ///     taking into account the distance between the branches and the offset.
        /// </param>
        /// <returns>
        ///     If the result is false, then abort the process
        ///     (without shifting the current node and all previously passed).
        /// </returns>
        private bool ShiftPerson(TreeChartPerson person, int offset, bool ext, bool verify = false)
        {
            if (person == null || offset == 0) return true;

            int branchDist = GetBranchDistance(person);

            // fix #189
            if (verify && (person.Rect.Left + offset < GetEdge(person, true) + branchDist)) {
                return false;
            }

            if (ext && fExtendedTree && person.HasFlag(PersonFlag.pfRootSpouse)) {
                return ShiftSpousesFrom(person, offset);
            } else if (ext && person.BaseSpouse != null && (person.BaseSpouse.Sex == GDMSex.svFemale || person.BaseSpouse.GetSpousesCount() == 1)) {
                if (!ShiftPerson(person.BaseSpouse, offset, true, verify))
                    return false;
            } else {
                if (person.Generation <= 0) {
                    // shifts the parents of the central person
                    // following its shifts due to the growth of branches of descendants

                    if (!ShiftPerson(person.Father, offset, false, verify))
                        return false;

                    if (!ShiftPerson(person.Mother, offset, false, verify))
                        return false;
                } else {
                    // following the shifts of a person due to the growth of branches of descendants,
                    // shifts that of her parents, along which the branch came

                    TreeChartPerson parent = null;
                    if (person.HasFlag(PersonFlag.pfDescByFather)) {
                        parent = person.Father;
                    } else if (person.HasFlag(PersonFlag.pfDescByMother)) {
                        parent = person.Mother;
                    }
                    if (!ShiftPerson(parent, offset, true, verify))
                        return false;
                }
            }

            person.PtX += offset;
            return true;
        }

        /// <summary>
        /// Children are always attached to the spouse of the person through whom the bypass/calculation came.
        /// </summary>
        private void RecalcDescChilds(TreeChartPerson person)
        {
            if (person.IsCollapsed) return;

            int childrenCount = person.GetChildsCount();
            if (childrenCount == 0) return;

            int centX = 0;

            // If the entry person has one spouse, the children's lines come from the midpoint between the spouses (alignPair case).
            // If the entry person has several spouses, the children's lines come from the corresponding spouse.
            bool alignPair = person.BaseSpouse != null && person.BaseSpouse.GetSpousesCount() == 1;
            if (alignPair) {
                switch (person.Sex) {
                    case GDMSex.svMale:
                        centX = (person.Rect.Right + person.BaseSpouse.Rect.Left) / 2;
                        break;

                    case GDMSex.svFemale:
                        centX = (person.BaseSpouse.Rect.Right + person.Rect.Left) / 2;
                        break;
                }
            } else {
                centX = person.PtX;
            }

            // the total width of the area that includes all children
            int childrenWidth = (childrenCount - 1) * fBranchDistance;
            for (int i = 0; i < childrenCount; i++) {
                childrenWidth += person.GetChild(i).Width;
            }

            int curX = centX - childrenWidth / 2;
            int curY = NextGenY(person, false);

            for (int i = 0; i < childrenCount; i++) {
                TreeChartPerson child = person.GetChild(i);
                RecalcDesc(child, curX + child.Width / 2, curY, true);
                curX = child.Rect.Right + fBranchDistance;
            }

            #region deprecated
            // This code is designed to align parents in the center of the location of children (across width),
            // because in the process of drawing children, various kinds of displacement are formed,
            // and the initial arrangement of the parents can be very laterally,
            // after the formation of a complete tree of their descendants.
            // However, this may be a problem (reason of #189) in the case if a shift initiated from descendants,
            // must be performed to the left with an overlay on an already formed side branch.

            /*if (IsExtPerson(person)) return;

            curX = person.GetChild(0).PtX;
            if (childrenCount > 1) {
                curX += (person.GetChild(childrenCount - 1).PtX - curX) / 2;
            }

            // FIXME: displacement #1
            if (alignPair) {
                int offset;
                switch (person.Sex) {
                    case GDMSex.svMale:
                        // fix #189
                        offset = curX - (fBranchDistance + person.Width) / 2 + 1 - person.PtX;
                        if (person.Rect.Left + offset < GetEdge(person, false)) {
                            return;
                        }

                        ShiftPerson(person, curX - (fBranchDistance + person.Width) / 2 + 1 - person.PtX, true);
                        ShiftPerson(person.BaseSpouse, curX + (fBranchDistance + person.BaseSpouse.Width) / 2 - person.BaseSpouse.PtX, true);
                        break;

                    case GDMSex.svFemale:
                        // fix #189
                        offset = curX - (fBranchDistance + person.BaseSpouse.Width) / 2 + 1 - person.BaseSpouse.PtX;
                        if (person.BaseSpouse.Rect.Left + offset < GetEdge(person.BaseSpouse, false)) {
                            return;
                        }

                        ShiftPerson(person, curX + (fBranchDistance + person.Width) / 2 - person.PtX, true);
                        ShiftPerson(person.BaseSpouse, curX - (fBranchDistance + person.BaseSpouse.Width) / 2 + 1 - person.BaseSpouse.PtX, true);
                        break;
                }
            } else {
                ShiftPerson(person, curX - person.PtX, true, true);
            }*/
            #endregion
        }

        private bool IsExtRootSpouse(TreeChartPerson person)
        {
            return fExtendedTree && person != null && person.HasFlag(PersonFlag.pfRootSpouse);
        }

        private void RecalcDesc(TreeChartPerson person, int ptX, int ptY, bool predef)
        {
            if (person == null) return;

            person.IsVisible = true;

            if (predef && !IsExtRootSpouse(person)) {
                person.SetPt(ptX, ptY);
            }

            int bound = GetCurrentBranchLeftBound(person, false);
            if (person.Rect.Left < bound) {
                ShiftPerson(person, bound - person.Rect.Left, true);
            }

            // the man's spouses align to his right
            if (person.Sex == GDMSex.svMale) {
                RecalcDescChilds(person);
                SetEdge(person);
            }

            int spousesCount = person.GetSpousesCount();
            if (spousesCount > 0) {
                TreeChartPerson prev = person;
                for (int i = 0; i < spousesCount; i++) {
                    TreeChartPerson sp = person.GetSpouse(i);
                    int branchDist = GetBranchDistance(sp);

                    // position of spouses by default
                    // without regard to other factors and subsequent children
                    int spX = 0;
                    int spOffset = (branchDist + sp.Width / 2);
                    switch (sp.Sex) {
                        case GDMSex.svMale:
                            spX = prev.Rect.Left - spOffset;
                            break;

                        case GDMSex.svFemale:
                            spX = prev.Rect.Right + spOffset;
                            break;
                    }

                    RecalcDesc(sp, spX, ptY, true);

                    // spouses arranged from first to last from left to right
                    // therefore for several wifes of one man, the previous node is the previous wife
                    // however, for several husbands of one woman, the previous node is a woman
                    if (sp.Sex == GDMSex.svFemale) {
                        prev = sp;
                    }
                }
            }

            // the woman's spouses align to her left
            if (person.Sex == GDMSex.svFemale) {
                RecalcDescChilds(person);
                SetEdge(person);
            }

            // FIXME: Temporary hack: if this person does not specify a particular sex,
            // then breaks the normal sequence of formation of coordinates.
            if (person.Sex == GDMSex.svUnknown || person.Sex == GDMSex.svIntersex) {
                SetEdge(person);
            }

            #region deprecated
            // Fix of long-distance displacement of male nodes in the presence of more than
            // one marriage and a large tree of descendants from the first wife.
            // Warning: hard breaks ancestors location in case of extended tree mode!
            // FIXME: displacement #2
            /*if (!IsExtendedTree() && (person.Sex == GDMSex.svMale && spousesCount >= 2)) {
                var firstWife = person.GetSpouse(0);
                if (firstWife.GetChildsCount() > 0) {
                    int d = firstWife.Rect.Left - person.Rect.Right;
                    if (d > fBranchDistance * 1.5f) {
                        //person.SetFlag(PersonFlag.pfSpecialMark);
                        int offset = (d - fBranchDistance);
                        ShiftPerson(person, offset, true);
                    }
                }
            }*/
            #endregion
        }

        private void RecalcDescendantsChart(bool predef)
        {
#if !DESK_METHOD
            ClearEdges();
#endif
            RecalcDesc(fRoot, fMargins, fMargins, predef);
        }

        #endregion
    }
}
