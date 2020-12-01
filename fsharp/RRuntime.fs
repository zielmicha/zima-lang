namespace Roboot.Runtime

type RObject =
    abstract RType : int64

    abstract FieldNames : int64 array
    abstract FieldValues : obj array

    // returns [null] if field is missing
    abstract GetField : id:int64 -> System.Object

    abstract WithField : id:int64 * newvalue:obj -> RObject

type DynObject(rType: int64, fieldNames : int64 array, fieldValues : obj array) =
    override this.ToString() =
        let a = Array.zip fieldNames fieldValues |> Array.toList |> List.map (fun (name, value) -> sprintf "%s=%s" (name.ToString()) (value.ToString()))
        sprintf "%d{%s}" rType (String.concat ", " a)

    interface RObject with
        member this.FieldNames = fieldNames
        member this.FieldValues = fieldValues
        member this.RType = rType

        member this.GetField(id) =
            let mutable result = null
            for i in 0 .. fieldNames.Length - 1 do
                if fieldNames.[i] = id then
                    result <- fieldValues.[i]
            result

        member this.WithField(id: int64, newValue: obj) =
            let mutable hasField = false
            for i in 0 .. fieldNames.Length - 1 do
                if fieldNames.[i] = id then hasField <- true

            if not hasField then
                let newFieldNames = Array.create (fieldNames.Length + 1) 0L
                let newValues = Array.create (fieldNames.Length + 1) null
                let mutable src = 0
                let mutable dst = 0
                while dst < newFieldNames.Length do
                    if src = dst && fieldNames.[src] > id then
                        newFieldNames.[dst] <- id
                        newValues.[dst] <- newValue
                        dst <- dst + 1
                    if src < fieldNames.Length then
                        newFieldNames.[dst] <- fieldNames.[src]
                        newValues.[dst] <- fieldValues.[src]
                        src <- src + 1
                        dst <- dst + 1
                upcast DynObject(rType, newFieldNames, newValues)
            else
                let newValues = Array.copy fieldValues
                for i in 0 .. fieldNames.Length - 1 do
                    if fieldNames.[i] = id then
                        newValues.[i] <- newValue
                upcast DynObject(rType, fieldNames, newValues)

type Util() =
    static member RaiseUnexpectedVariant() =
        failwith "unexpected variant"

type Context() =
    static member currentContext = new System.Threading.ThreadLocal<RObject>()

    static member Get() = Context.currentContext.Value

    static member Set(v: RObject) = Context.currentContext.Value <- v
