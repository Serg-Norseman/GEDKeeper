/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

namespace GKCore.Stats
{
    /// <summary>
    /// Types of statistics.
    /// </summary>
    public enum StatsMode
    {
        smAncestors,
        smDescendants,
        smDescGenerations,
        smSurnames,
        smNames,
        smPatronymics,
        smAge,
        smLifeExpectancy,
        smBirthYears,
        smBirthTenYears,
        smDeathYears,
        smDeathTenYears,
        smChildsCount,
        smChildsDistribution,
        smBirthPlaces,
        smDeathPlaces,
        smResidences,
        smOccupation,
        smReligious,
        smNational,
        smEducation,
        smCaste,
        smFirstbornAge,
        smMarriages,
        smMarriageAge,
        smSpousesDiff,
        smHobby,
        smAward,
        smMili,
        smMiliInd,
        smMiliDis,
        smMiliRank,
        smAAF_1,
        smAAF_2,
        smCertaintyIndex,
        smBirthByMonth,
        smDemography,
        smParentsAge,
        
        smLast = smParentsAge
    }
}
