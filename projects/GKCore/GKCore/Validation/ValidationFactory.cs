/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using GDModel;
using GKCore.Utilities;
using GKCore.Validation.Concrete;

namespace GKCore.Validation
{
    public static class ValidationFactory
    {
        public static void InitGDMValidators()
        {
            // FIXME: temp place, move
            var container = AppHost.Container;
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
