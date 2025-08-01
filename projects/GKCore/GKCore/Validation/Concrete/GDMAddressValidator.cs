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
