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
    public class GDMUserReferenceValidator : BaseValidator<GDMUserReference>
    {
        public override ValidationResult Validate(GDMUserReference obj, bool suppressWarnings = false)
        {
            if (obj == null)
                throw new ArgumentNullException(nameof(obj));

            var result = new ValidationResult();

            if (!CheckMaximumLength(obj.StringValue, GEDCOMConsts.URef_Number_MaxLength))
                result.AddError(string.Format(LangMan.LS(LSID.FieldIsLonger), LangMan.LS(LSID.Reference), GEDCOMConsts.URef_Number_MaxLength));

            if (!CheckMaximumLength(obj.ReferenceType, GEDCOMConsts.URef_Type_MaxLength))
                result.AddError(string.Format(LangMan.LS(LSID.FieldIsLonger), LangMan.LS(LSID.Type), GEDCOMConsts.URef_Type_MaxLength));

            return result;
        }
    }
}
