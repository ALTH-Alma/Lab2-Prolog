

%Función extra:
 esListaString([]).
 esListaString([H|T]):- string(H), esListaString(T).


%_________________________________________

%TDA fecha: se usa para tener un modelo estandar para todas fechas que se usaran en los demas TDAs.
%Representación: fecha[entero dia, entero mes,entero año]

%Capa constructor:
fecha(D,M,A,F):- 
number(D), number(M), number(A),
D > 0, D < 32, M > 0, M < 13,
F=[D,M,A].


%Capa pertenencia:
esFecha([D,M,A]):- 
number(D), number(M), number(A),
D > 0, D < 32, M > 0, M < 13.

%Capa selector:
getDia([D,M,A],D):- esFecha([D,M,A]).
getMes([D,M,A],M):- esFecha([D,M,A]).
getAno([D,M,A],A):- esFecha([D,M,A]).


%_________________________________________

%TDA recompensa: se usa para representar una recompensa en una pregunta.
%representación: recompensa[string nombre oferto, entero monto].

%Capa constructor:
%recompensaVacia= ["",0];

recompensa(O,MR,R):- 
string(O), number(MR), 
R= [O,MR].

%Capa pertenencia:
esRecompensa([O,MT]):-
string(O), number(MT), 
MT>0; ([O,MT]==["",0]).

%Capa selector:
getOfertor([O,MT],O):- esRecompensa([O,MT]).
getMontoRecompensa([O,MT],MT):- esRecompensa([O,MT]).


%_________________________________________

%TDA usuario: representa un usuario y sus elementos obligatorios.
%Representación: usuario[string nombre, string contraseña, entero reputación, lista de string referencias].

%Capa constructor: 
%Entrada: un string NU (nombre del usuario), un string CU (contraseña del usuario), un número mayor que 0 (reputación del usuario),
%una lista de strings LR (lista de referencias del usuario) y una variable para asignar al usuario.
%Salida: un Usuario (lista de elementos) si las entradas son validas y un false sino.

usuario(NU,CU,RU,LR, USUARIO):-
string(NU), string(CU), number(RU), esListaString(LR),
RU > -1,
USUARIO= [NU,CU,RU,LR].

%Ejemplo U1= ["Maria", "Maria1999", 50, ["Racket","c#"]].
%Ejemplo U2= ["Ana","A1234", 70, ["java","python"]].
%Ejemplo U3= ["Juan","juan2000", 20, ["python","c++"]].
%Ejemplo U4= ["Pedro", "P340", 90,  ["python","c++"]].

%Capa pertenencia:
%Entrada: una lista de elementos que sirve como postulante a un usuario.
%Salida: un booleano, un trus si la lista es un usuario y un false sino.

esUsuario([NU,CU,RU,LR]):-
string(NU), string(CU), number(RU), esListaString(LR),
RU > -1 .

%Capa selector:
%Entrada: todas los predicados de la capa selector reciben un usuario y una variable para guardar el dato buscado.

%Salida: un string, nombre del usuario.
getNameUser([NU,CU,RU,LR],NU):- esUsuario([NU,CU,RU,LR]).

%Entrada: un string, contraseña del usuario.
getPassUser([NU,CU,RU,LR],CU):- esUsuario([NU,CU,RU,LR]).

%Salida: un número mayor que 0, puntos de reputación del usuario.
getReputUser([NU,CU,RU,LR],RU):- esUsuario([NU,CU,RU,LR]).

%Salida: una lista de strings, una lista de las referencias del usuario.
getRefeUser([NU,CU,RU,LR],LR):- esUsuario([NU,CU,RU,LR]).


%_________________________________________

%TDA usuarios: representan una lista de usuarios en el stack.
%Representación: una listade usuarios [usuario1, usuario2, ......., usuarioN].

%Capa pertenencia:
%Entrada: una lista con elementos, sirve como postulante para una lista de usuarios.
%Salida: un booleano, un trus si la lista corresponde a una lista de usuarios.
esListaUsuarios([]).
esListaUsuarios([H|T]):- esUsuario(H),esListaUsuarios(T).

%Capa constructor:
%Entrada: una lista de usuarios o una nueva lista vacia, y un nuevo usuario.
%Salida: un nueva lista de usuarios.
agregarUsuario([], [NU,CU,RU,LR], [[NU,CU,RU,LR]]):- esUsuario([NU,CU,RU,LR]). %caso base
agregarUsuario([H|T], [NU,CU,RU,LR],  [H|NEWT]):- 
esListaUsuarios([H|T]),
agregarUsuario(T, [NU,CU,RU,LR], NEWT). 

%Ejemplo: [["Maria", "Maria1999", 50, ["Racket","c#"]],["Ana","A1234", 70, ["java","python"]],["Juan","juan2000", 20, ["python","c++"]]].

%Capa selector: entrega a un usuario en una lista de usuarios al idendificarlo por su nombre.
getUsuario([], NOMBRE, H).

getUsuario([H|T], NOMBRE,  NOM):- 
getNameUser(H,NU), NU==NOMBRE, NOM=H, !, getUsuario(T,NOMBRE, H).

%getUsuario([H|T], NOMBRE, H):-
%getUsuario(T, NOMBRE, H).


%_________________________________________

%TDA respuesta: representa una respuesta y sus elementos obligatorios.
%Representación: respuesta[entero id respuesta, string autor, fecha fecha de publicación, string contenido respuesta, lista etiquetas, string estado (Aceptada/Rechazada), entero votos a favor,
%enteros votos en contra, entero reportes].

%Capa constructor:
%Entrada: entero ID (identificador pregunta), string AR (autor de la respuesta), fecha FP (fecha de publicación), string C (contenido), lista de strings LE (lista de etiquetas),
%string EA (estado de aceptacion), entero VF (votos a favor), entero VC (votos en contra), entero NR (número de reportes) y una variable RESPUESTA para asignar la respuesta contruida.
%Salida: un Respuesta (lista de elementos) si las entradas son correctas y un false sino.

respuesta(ID, AR, FP, C, LE, EA, VF, VC, NR, RESPUESTA):-
number(ID), string(AR), esFecha(FP), string(C), esListaString(LE), string(EA), number(VF), number(VC), number(NR), 
ID > -1, VF > -1, VC > -1, NR > -1,
RESPUESTA=[ID, AR, FP, C, LE, EA, VF, VC, NR, RESPUESTA].

%Ejemplo R1= [0,"Pedro", [2,3,2020], "Aumenta la complejidad y puede generar resultados impredecibles", ["Variables globales", "Problemas"], "Aceptada",5,2,0].

%Capa pertenencia:
%Entrada: una lista de elementos que sirve como postulante a ser una respuesta.
%Salida: un booleano, un true si la lista es una respuesta y un false sino.

esRespuesta([ID, AR, FP, C, LE, EA, VF, VC, NR]):-
number(ID), string(AR), esFecha(FP), string(C), esListaString(LE), string(EA), number(VF), number(VC), number(NR).

%Capa selector:
%Entrada: todas los predicados de la capa selector reciben una respuesta y una variable para guardar el dato buscado.

%Salida: un número, ID de la respuesta.
getIDRes([ID, AR, FP, C, LE, EA, VF, VC, NR],ID):- esRespuesta([ID, AR, FP, C, LE, EA, VF, VC, NR]).

%Salida: un string, nombre del autor de la respuesta.
getAutorRes([ID, AR, FP, C, LE, EA, VF, VC, NR],AR):- esRespuesta([ID, AR, FP, C, LE, EA, VF, VC, NR]).

%Salida: una fecha, fecha de publicación de la respuesta.
getFechaRes([ID, AR, FP, C, LE, EA, VF, VC, NR],FP):- esRespuesta([ID, AR, FP, C, LE, EA, VF, VC, NR]).

%Salida: un string, contenido de la respuesta.
getContenidoRes([ID, AR, FP, C, LE, EA, VF, VC, NR],C):- esRespuesta([ID, AR, FP, C, LE, EA, VF, VC, NR]).

%Salida: un lista de strings, lista de etiquetas de la respuesta.
getListaDeEtiquetasRes([ID, AR, FP, C, LE, EA, VF, VC, NR],LE):- esRespuesta([ID, AR, FP, C, LE, EA, VF, VC, NR]).

%Salida: un string, estado de aceptación de la respuesta.
getEstadoRes([ID, AR, FP, C, LE, EA, VF, VC, NR],EA):- esRespuesta([ID, AR, FP, C, LE, EA, VF, VC, NR]).

%Salida: un número, votos a favor de la respuesta.
getVotosAFavorRes([ID, AR, FP, C, LE, EA, VF, VC, NR],VF):- esRespuesta([ID, AR, FP, C, LE, EA, VF, VC, NR]).

%Salida: un número, votos en contra de la respuesta.
getVotosEnContraRes([ID, AR, FP, C, LE, EA, VF, VC, NR],VC):- esRespuesta([ID, AR, FP, C, LE, EA, VF, VC, NR]).

%Salida: un número, reportes de la respuesta.
getReportesRes([ID, AR, FP, C, LE, EA, VF, VC, NR],NR):- esRespuesta([ID, AR, FP, C, LE, EA, VF, VC, NR]).


%_________________________________________

%TDA pregunta: representa una preguntas y sus lementos obligatorios.
%Representación: pregunta[]


