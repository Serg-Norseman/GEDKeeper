/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
using System.Reflection;

namespace GKCore.IoC
{
    public class TypeNotRegisteredException : Exception
    {
        public TypeNotRegisteredException(string message) : base(message)
        {
        }
    }


    public enum LifeCycle
    {
        Singleton,
        Transient
    }


    public interface IContainer
    {
        void Register<TTypeToResolve, TConcrete>();
        void Register<TTypeToResolve, TConcrete>(LifeCycle lifeCycle, bool canReplace = false);
        void Reset();
        TTypeToResolve Resolve<TTypeToResolve>(params object[] parameters);
        object Resolve(Type typeToResolve, params object[] parameters);
    }


    internal class RegisteredObject
    {
        public Type TypeToResolve { get; private set; }

        public Type ConcreteType { get; private set; }

        public object Instance { get; private set; }

        public LifeCycle LifeCycle { get; private set; }

        public RegisteredObject(Type typeToResolve, Type concreteType, LifeCycle lifeCycle)
        {
            TypeToResolve = typeToResolve;
            ConcreteType = concreteType;
            LifeCycle = lifeCycle;
        }

        public void CreateInstance(params object[] args)
        {
            Instance = Activator.CreateInstance(ConcreteType, args);
        }
    }


    public class IocContainer : IContainer
    {
        private readonly IDictionary<Type, RegisteredObject> fRegisteredObjects = new Dictionary<Type, RegisteredObject>();

        public void Reset()
        {
            fRegisteredObjects.Clear();
        }

        public void Register<TTypeToResolve, TConcrete>()
        {
            Register<TTypeToResolve, TConcrete>(LifeCycle.Singleton);
        }

        public void Register<TTypeToResolve, TConcrete>(LifeCycle lifeCycle, bool canReplace = false)
        {
            Type typeToResolve = typeof(TTypeToResolve);

            if (fRegisteredObjects.ContainsKey(typeToResolve)) {
                if (!canReplace) {
                    throw new TypeNotRegisteredException(string.Format(
                        "The type {0} has been registered and can't replaced", typeToResolve.Name));
                } else {
                    fRegisteredObjects.Remove(typeToResolve);
                }
            }

            fRegisteredObjects.Add(typeToResolve, new RegisteredObject(typeToResolve, typeof(TConcrete), lifeCycle));
        }

        public TTypeToResolve Resolve<TTypeToResolve>(params object[] parameters)
        {
            return (TTypeToResolve) Resolve(typeof(TTypeToResolve), parameters);
        }

        public object Resolve(Type typeToResolve, params object[] parameters)
        {
            RegisteredObject registeredObject;
            if (!fRegisteredObjects.TryGetValue(typeToResolve, out registeredObject)) {
                throw new TypeNotRegisteredException(string.Format(
                    "The type {0} has not been registered", typeToResolve.Name));
            }
            return GetInstance(registeredObject, parameters);
        }

        private object GetInstance(RegisteredObject registeredObject, params object[] parameters)
        {
            if (registeredObject.Instance == null || registeredObject.LifeCycle == LifeCycle.Transient) {
                if (parameters == null || parameters.Length == 0) {
                    Type implementation = registeredObject.ConcreteType;
                    ConstructorInfo constructor = implementation.GetConstructors()[0];
                    ParameterInfo[] constructorParameters = constructor.GetParameters();

                    int paramsLength = constructorParameters.Length;
                    parameters = new object[paramsLength];
                    if (paramsLength > 0) {
                        for (int i = 0; i < paramsLength; i++) {
                            ParameterInfo parameterInfo = constructorParameters[i];
                            parameters[i] = Resolve(parameterInfo.ParameterType);
                        }
                    }
                }

                registeredObject.CreateInstance(parameters);
            }

            return registeredObject.Instance;
        }
    }
}
