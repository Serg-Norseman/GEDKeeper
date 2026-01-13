/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Locales;

namespace GKCore.Validation.Concrete
{
    public class GDMEventValidator : BaseValidator<GDMCustomEvent>
    {
        public override ValidationResult Validate(GDMCustomEvent obj, bool suppressWarnings = false)
        {
            if (obj == null)
                throw new ArgumentNullException(nameof(obj));

            var result = new ValidationResult();

            if (!CheckMaximumLength(obj.Cause, GEDCOMConsts.Event_Cause_MaxLength))
                result.AddError(string.Format(LangMan.LS(LSID.FieldIsLonger), LangMan.LS(LSID.Cause), GEDCOMConsts.Event_Cause_MaxLength));

            if (!CheckMaximumLength(obj.Agency, GEDCOMConsts.Event_Agency_MaxLength))
                result.AddError(string.Format(LangMan.LS(LSID.FieldIsLonger), LangMan.LS(LSID.Agency), GEDCOMConsts.Event_Agency_MaxLength));

            return result;
        }
    }
}
