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
using GDModel;
using GKCore.IoC;
using GKCore.Validation.Concrete;

namespace GKCore.Validation
{
    public static class ValidationFactory
    {
        public static void InitGDMValidators()
        {
            // FIXME: temp place, move
            IContainer container = AppHost.Container;
            container.Register<IValidator<GDMAddress>, GDMAddressValidator>(LifeCycle.Singleton);
            container.Register<IValidator<GDMCustomEvent>, GDMEventValidator>(LifeCycle.Singleton);
            container.Register<IValidator<GDMRepositoryRecord>, GDMRepositoryRecordValidator>(LifeCycle.Singleton);
            container.Register<IValidator<GDMSourceCallNumber>, GDMSourceCallNumberValidator>(LifeCycle.Singleton);
            container.Register<IValidator<GDMSourceRecord>, GDMSourceRecordValidator>(LifeCycle.Singleton);
            container.Register<IValidator<GDMUserReference>, GDMUserReferenceValidator>(LifeCycle.Singleton);
        }

        public static ValidationResult Validate<T>(T obj)
        {
            try {
                var validator = AppHost.Container.Resolve<IValidator<T>>();
                return validator.Validate(obj);
            } catch (TypeNotRegisteredException) {
                return ValidationResult.Empty;
            } catch (Exception ex) {
                var messages = new List<ValidationMessage> { new ValidationMessage { Message = string.Format("Error validating {0}", obj) } };
                messages.AddRange(FlattenError(ex));
                var result = new ValidationResult { Messages = messages };
                return result;
            }
        }

        private static IEnumerable<ValidationMessage> FlattenError(Exception exception)
        {
            var messages = new List<ValidationMessage>();
            var currentException = exception;

            do {
                messages.Add(new ValidationMessage { Message = exception.Message });
                currentException = currentException.InnerException;
            } while (currentException != null);

            return messages;
        }
    }
}
