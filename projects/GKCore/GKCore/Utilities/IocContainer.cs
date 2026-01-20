/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using System.Reflection;

namespace GKCore.Utilities
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


    public sealed class IocContainer
    {
        private readonly IDictionary<Type, RegisteredObject> fRegisteredObjects = new Dictionary<Type, RegisteredObject>();

        public void Reset()
        {
            fRegisteredObjects.Clear();
        }

        public void Register<TTypeToResolve, TConcrete>(LifeCycle lifeCycle = LifeCycle.Singleton, bool canReplace = false)
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
            return (TTypeToResolve)Resolve(typeof(TTypeToResolve), parameters);
        }

        public TTypeToResolve TryResolve<TTypeToResolve>(params object[] parameters)
        {
            Type typeToResolve = typeof(TTypeToResolve);

            RegisteredObject registeredObject;
            if (!fRegisteredObjects.TryGetValue(typeToResolve, out registeredObject)) {
                return default(TTypeToResolve);
            }
            return (TTypeToResolve)GetInstance(registeredObject, parameters);
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
                            var paramType = constructorParameters[i].ParameterType;
                            parameters[i] = Resolve(paramType);
                        }
                    }
                }

                registeredObject.CreateInstance(parameters);
            }

            return registeredObject.Instance;
        }
    }
}
