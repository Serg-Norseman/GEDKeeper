using System;
using NUnit.Framework;

namespace GKCore.IoC
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

            Assert.Throws(typeof(TypeNotRegisteredException), () => {
                              container.Register<ITypeToResolve, ConcreteType>(LifeCycle.Singleton, false);
                          });

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
