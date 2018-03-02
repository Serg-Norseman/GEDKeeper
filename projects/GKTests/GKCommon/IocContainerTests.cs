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
using GKCore.IoC;
using NUnit.Framework;

namespace GKTests.GKCommon
{
    [TestFixture]
    public class IocContainerTests
    {
        [Test]
        public void Test_ResolveObject()
        {
            var container = new IocContainer();
            container.Register<ITypeToResolve, ConcreteType>();

            var instance = container.Resolve<ITypeToResolve>();
            Assert.IsInstanceOf(typeof(ConcreteType), instance);
        }

        [Test]
        public void Test_RegisterAndReplace()
        {
            var container = new IocContainer();
            container.Register<ITypeToResolve, ConcreteType>();

            container.Register<ITypeToResolve, ConcreteType>(LifeCycle.Singleton, false);
            container.Register<ITypeToResolve, ConcreteType>(LifeCycle.Singleton, true);

            var instance = container.Resolve<ITypeToResolve>();
            Assert.IsInstanceOf(typeof(ConcreteType), instance);
        }

        [Test]
        public void Test_ExceptionIfNotRegistered()
        {
            var container = new IocContainer();

            Exception exception = null;
            try
            {
                container.Resolve<ITypeToResolve>();
            }
            catch (Exception ex)
            {
                exception = ex;
            }

            Assert.IsInstanceOf(typeof(TypeNotRegisteredException), exception);
        }

        [Test]
        public void Test_ResolveWithConstructorParameters()
        {
            var container = new IocContainer();
            container.Register<ITypeToResolve, ConcreteType>();
            container.Register<ITypeToResolveWithConstructorParams, ConcreteTypeWithConstructorParams>();

            var instance = container.Resolve<ITypeToResolveWithConstructorParams>();
            Assert.IsInstanceOf(typeof(ConcreteTypeWithConstructorParams), instance);
        }

        [Test]
        public void Test_CreateSingletonInstance()
        {
            var container = new IocContainer();
            container.Register<ITypeToResolve, ConcreteType>();

            var instance = container.Resolve<ITypeToResolve>();
            Assert.That(container.Resolve<ITypeToResolve>(), Is.SameAs(instance));
        }

        [Test]
        public void Test_CreateTransientInstance()
        {
            var container = new IocContainer();
            container.Register<ITypeToResolve, ConcreteType>(LifeCycle.Transient);

            var instance = container.Resolve<ITypeToResolve>();
            Assert.That(container.Resolve<ITypeToResolve>(), Is.Not.SameAs(instance));
        }
    }

    public interface ITypeToResolve
    {
    }

    public class ConcreteType : ITypeToResolve
    {
    }

    public interface ITypeToResolveWithConstructorParams
    {
    }

    public class ConcreteTypeWithConstructorParams : ITypeToResolveWithConstructorParams
    {
        public ConcreteTypeWithConstructorParams(ITypeToResolve typeToResolve)
        {
        }
    }
}
