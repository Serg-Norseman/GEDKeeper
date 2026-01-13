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
    public class GDMAddressValidator : BaseValidator<GDMAddress>
    {
        public override ValidationResult Validate(GDMAddress obj, bool suppressWarnings = false)
        {
            if (obj == null)
                throw new ArgumentNullException(nameof(obj));

            var result = new ValidationResult();

            if (!CheckMaximumLength(obj.AddressCity, GEDCOMConsts.Address_City_MaxLength))
                result.AddError(string.Format(LangMan.LS(LSID.FieldIsLonger), LangMan.LS(LSID.AdCity), GEDCOMConsts.Address_City_MaxLength));

            if (!CheckMaximumLength(obj.AddressCountry, GEDCOMConsts.Address_Country_MaxLength))
                result.AddError(string.Format(LangMan.LS(LSID.FieldIsLonger), LangMan.LS(LSID.AdCountry), GEDCOMConsts.Address_Country_MaxLength));

            if (!CheckMaximumLength(obj.AddressPostalCode, GEDCOMConsts.Address_PostalCode_MaxLength))
                result.AddError(string.Format(LangMan.LS(LSID.FieldIsLonger), LangMan.LS(LSID.AdPostalCode), GEDCOMConsts.Address_PostalCode_MaxLength));

            if (!CheckMaximumLength(obj.AddressState, GEDCOMConsts.Address_State_MaxLength))
                result.AddError(string.Format(LangMan.LS(LSID.FieldIsLonger), LangMan.LS(LSID.AdState), GEDCOMConsts.Address_State_MaxLength));

            return result;
        }
    }
}
