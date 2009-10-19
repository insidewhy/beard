package org.beard.config

abstract class ValueAbstract {
    def apply(argIt:CmdLineIterator)
    def after(argIt:CmdLineIterator):Boolean
    def suffix:String = ""
}

abstract class Value[T](default:T) extends ValueAbstract {
    var value = default
}

object Value {
    def apply(arg:Boolean):BoolValue = {
        return new BoolValue(arg)
    }

    def apply(arg:String):StringValue = {
        return new StringValue(arg)
    }

    def apply(arg:Int):IntValue = {
        return new IntValue(arg)
    }
}

abstract class SwitchArgument[T](default:T) extends Value[T](default) {
    override def after(argIt:CmdLineIterator):Boolean = {
        apply(argIt)
        argIt.size > 0
    }
}

class BoolValue(default:Boolean) extends SwitchArgument[Boolean](default) {
    override def apply(argIt:CmdLineIterator) {
        value = true
    }
}

abstract class ValueArgument[T](default:T) extends Value[T](default) {
    override def apply(argIt:CmdLineIterator) {
        store(argIt.value)
        argIt += 1
    }

    override def after(argIt:CmdLineIterator):Boolean = {
        store(argIt.positionedValue)
        argIt += 1
        false
    }

    override def suffix:String = " <arg>"

    def store(argValue:String)
}


class StringValue(default:String) extends ValueArgument[String](default) {
    override def store(argValue:String) { value = argValue }
}

class IntValue(default:Int) extends ValueArgument[Int](default) {
    override def store(argValue:String) { value = argValue.toInt }
}

