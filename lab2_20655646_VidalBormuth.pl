

%FPredicado extra extra:
 esListaString([]).
 esListaString([H|T]):- string(H), esListaString(T).


%_________________________________________

%TDA fecha: se usa para tener un modelo estandar para todas fechas que se usaran en los demas TDAs.
%Representación: fecha[entero dia, entero mes,entero año]

%Capa constructor:
fecha(D,M,A,F):- 
integer(D), integer(M), integer(A),
D > 0, D < 32, M > 0, M < 13,
F=[D,M,A].


%Capa pertenencia:
esFecha([D,M,A]):- 
integer(D), integer(M), integer(A),
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
string(O), integer(MR), 
R= [O,MR].

%Capa pertenencia:
esRecompensa([O,MT]):-
string(O), integer(MT), 
MT>0; ([O,MT]==["",0]).

%Capa selector:
getOfertor([O,MT],O):- esRecompensa([O,MT]).
getMontoRecompensa([O,MT],MT):- esRecompensa([O,MT]).


%_________________________________________

%TDA usuario: representa un usuario y sus elementos obligatorios.
%Representación: usuario[string nombre, string contraseña, entero reputación, lista de string referencias].

%Capa constructor: 
%Entrada: un string NU (nombre del usuario), un string CU (contraseña del usuario), un entero mayor que 0 (reputación del usuario),
%una lista de strings LR (lista de referencias del usuario) y una variable para asignar al usuario.
%Salida: un Usuario (lista de elementos) si las entradas son validas y un false sino.

usuario([]).
crearUsuario(NU,CU,RU,LR, USUARIO):-
string(NU), string(CU), integer(RU), esListaString(LR),
RU > -1,
USUARIO= [NU,CU,RU,LR].

%Ejemplo U1= ["Maria", "Maria1999", 50, ["Racket","c#"]].
%Ejemplo U2= ["Ana","A1234", 70, ["java","python"]].
%Ejemplo U3= ["Juan","juan2000", 20, ["python","c++"]].
%Ejemplo U4= ["Pedro", "P340", 90,  ["python","c++"]].

%Capa pertenencia:
%Entrada: una lista de elementos que sirve como postulante a un usuario.
%Salida: un booleano, un trus si la lista es un usuario y un false sino.

esUsuario([]).
esUsuario([NU,CU,RU,LR]):-
string(NU), string(CU), integer(RU), esListaString(LR),
RU > -1 .

%Capa selector:
%Entrada: todas los predicados de la capa selector reciben un usuario y una variable para guardar el dato buscado.

%Salida: un string, nombre del usuario.
getNameUser([NU,CU,RU,LR],NU):- esUsuario([NU,CU,RU,LR]).

%Entrada: un string, contraseña del usuario.
getPassUser([NU,CU,RU,LR],CU):- esUsuario([NU,CU,RU,LR]).

%Salida: un entero mayor que 0, puntos de reputación del usuario.
getReputUser([NU,CU,RU,LR],RU):- esUsuario([NU,CU,RU,LR]).

%Salida: una lista de strings, una lista de las referencias del usuario.
getRefeUser([NU,CU,RU,LR],LR):- esUsuario([NU,CU,RU,LR]).


%_________________________________________

%TDA lista de usuarios: representa una lista de usuarios en el stack.
%Representación: una lista de usuarios [usuario1, usuario2, ......., usuarioN].

%Capa pertenencia:
%Entrada: una lista con elementos, sirve como postulante para una lista de usuarios.
%Salida: un booleano, un true si la lista corresponde a una lista de usuarios.

esListaUserVacia([]).
esListaUsuarios([]).
esListaUsuarios([H|T]):- esUsuario(H),esListaUsuarios(T).


%Capa modificador:
%Entrada: una lista de usuarios o una nueva lista vacia, el nuevo usuario y una variable para asignar la nueva lista de usuarios con el usuario incluido.
%Salida: un nueva lista de usuarios.

agregarUsuario([], [NU,CU,RU,LR], [[NU,CU,RU,LR]]):- esUsuario([NU,CU,RU,LR]). %caso base
agregarUsuario([H|T], [NU,CU,RU,LR], [H|NEWT]):- 
esListaUsuarios([H|T]),
agregarUsuario(T, [NU,CU,RU,LR], NEWT). 

%Ejemplo: [["Maria", "Maria1999", 50, ["Racket","c#"]],["Ana","A1234", 70, ["java","python"]],["Juan","juan2000", 20, ["python","c++"]]].

%Capa selector: entrega a un usuario en una lista de usuarios al idendificarlo por su nombre.
%Entrada: una lista de usuarios, un string NOMBRE (nombre del usuario que se desea obtener) y una variable U para asignar al usuario.
%Salida: un usuario.
getUsuario([[]|_], NOMBRE, U):- 
getNameUser(H,NU), NU == NOMBRE, U= H.

getUsuario([_|T], NOMBRE, U):-
getUsuario(T, NOMBRE, U).


%Función extra: verifica si existe un usuario en la lista de usuarios.
existeUsuario([[NOMBRE,_,_,_]|_], NOMBRE):- !.
existeUsuario([_|Usuarios], NOMBRE):-
existeUsuario(Usuarios, NOMBRE).


%_________________________________________

%TDA respuesta: representa una respuesta y sus elementos obligatorios.
%Representación: respuesta[entero id respuesta, string autor, fecha fecha de publicación, string contenido respuesta, lista etiquetas,
% string estado [Aceptada/Rechazada], entero votos a favor, entero votos en contra, entero reportes].

%Capa constructor:
%Entrada: entero IDR (identificador respuesta), string AR (autor de la respuesta), fecha FP (fecha de publicación), string C (contenido), 
%lista de strings LE (lista de etiquetas), string EA (estado de aceptacion), entero VF (votos a favor), entero VC (votos en contra), 
%entero NR (entero de reportes) y una variable RESPUESTA para asignar la respuesta contruida.
%Salida: un Respuesta (lista de elementos) si las entradas son correctas y un false sino.

respuesta([]).
crearRespuesta(IDR, AR, FP, C, LE, EA, VF, VC, NR, RESPUESTA):-
integer(IDR), string(AR), esFecha(FP), string(C), esListaString(LE), string(EA), integer(VF), integer(VC), integer(NR), 
IDR > -1, VF > -1, VC > -1, NR > -1,
RESPUESTA= [IDR, AR, FP, C, LE, EA, VF, VC, NR].

%Ejemplo R1= [0,"Pedro", [2,3,2020], "Aumenta la complejidad y puede generar resultados impredecibles", ["Variables globales", "Problemas"], "Aceptada",5,2,0].

%Capa pertenencia:
%Entrada: una lista de elementos que sirve como postulante a ser una respuesta.
%Salida: un booleano, un true si la lista es una respuesta y un false sino.

esRespuesta([]).
esRespuesta([IDR, AR, FP, C, LE, EA, VF, VC, NR]):-
integer(IDR), string(AR), esFecha(FP), string(C), esListaString(LE), string(EA), integer(VF), integer(VC), integer(NR),
IDR > -1, VF > -1, VC > -1, NR > -1 .

%Capa selector:
%Entrada: todas los predicados de la capa selector reciben una respuesta y una variable para guardar el dato buscado.

%Salida: un entero, ID de la respuesta.
getIdRes([IDR, AR, FP, C, LE, EA, VF, VC, NR],IDR):- esRespuesta([IDR, AR, FP, C, LE, EA, VF, VC, NR]).

%Salida: un string, nombre del autor de la respuesta.
getAutorRes([IDR, AR, FP, C, LE, EA, VF, VC, NR],AR):- esRespuesta([IDR, AR, FP, C, LE, EA, VF, VC, NR]).

%Salida: una fecha, fecha de publicación de la respuesta.
getFechaRes([IDR, AR, FP, C, LE, EA, VF, VC, NR],FP):- esRespuesta([IDR, AR, FP, C, LE, EA, VF, VC, NR]).

%Salida: un string, contenido de la respuesta.
getContenidoRes([IDR, AR, FP, C, LE, EA, VF, VC, NR],C):- esRespuesta([IDR, AR, FP, C, LE, EA, VF, VC, NR]).

%Salida: un lista de strings, lista de etiquetas de la respuesta.
getListaDeEtiquetasRes([IDR, AR, FP, C, LE, EA, VF, VC, NR],LE):- esRespuesta([IDR, AR, FP, C, LE, EA, VF, VC, NR]).

%Salida: un string, estado de aceptación de la respuesta.
getEstadoRes([IDR, AR, FP, C, LE, EA, VF, VC, NR],EA):- esRespuesta([IDR, AR, FP, C, LE, EA, VF, VC, NR]).

%Salida: un entero, votos a favor de la respuesta.
getVotosAFavorRes([IDR, AR, FP, C, LE, EA, VF, VC, NR],VF):- esRespuesta([IDR, AR, FP, C, LE, EA, VF, VC, NR]).

%Salida: un entero, votos en contra de la respuesta.
getVotosEnContraRes([IDR, AR, FP, C, LE, EA, VF, VC, NR],VC):- esRespuesta([IDR, AR, FP, C, LE, EA, VF, VC, NR]).

%Salida: un entero, reportes de la respuesta.
getReportesRes([IDR, AR, FP, C, LE, EA, VF, VC, NR],NR):- esRespuesta([IDR, AR, FP, C, LE, EA, VF, VC, NR]).

%_________________________________________

%TDA lista de respuestas: representa una lista de respuestas en una pregunta.
%Representación: una lista de respuestas [respuesta1, respuesta2, ......., respuestaN].

%Capa pertenencia:
%Entrada: una lista con elementos, sirve como postulante para una lista de respuestas.
%Salida: un booleano, un true si la lista corresponde a una lista de respuestas y un false sino.
esListaRespuestasVacia([]).
esListaRespuestas([]).
esListaRespuestas([H|T]):- esRespuesta(H),esListaRespuestas(T).

%Capa constructor:
%Entrada: una lista de respuestas o una nueva lista vacia, la nueva respuesta y una variable para asignar la nueva lista de respuestas con la respuesta incluida.
%Salida: una nueva lista de respuestas.

agregarRespuesta([], [IDR, AR, FP, C, LE, EA, VF, VC, NR], [[IDR, AR, FP, C, LE, EA, VF, VC, NR]]):- 
esRespuesta([IDR, AR, FP, C, LE, EA, VF, VC, NR]). %caso base

agregarRespuesta([H|T], [IDR, AR, FP, C, LE, EA, VF, VC, NR], [H|NEWT]):- 
esListaRespuestas([H|T]),
agregarRespuesta(T, [IDR, AR, FP, C, LE, EA, VF, VC, NR], NEWT). 

%Ejemplo: [[0,"Pedro", [2,3,2020], "Aumenta la complejidad y puede generar resultados impredecibles", ["Variables globales", "Problemas"], "Aceptada",5,2,0],[1, "Juan", [1, 3, 2020], "El problema de las variables globales es que crea dependencias ocultas. Cuando se trata de aplicaciones grandes, ni tú mismo sabes/recuerdas/tienes claro los objetos que tienes y sus relaciones.", ["Malas practicas","errores"], "", 2, 9, 1],[2, "Maria", [13, 11, 2020], "Usando Qt Style Sheet", [], "Rechazada", 6, 3, 0]].

%Capa selector: entrega una respuesta en una lista de respuesta al idendificarla por su id.
%Entrada: una lista de respuestas, un entero IDR (identificador de la respuesta que se desea obtener) y una variable R para asignar la respuesta.
%Salida: una respuestas.

getRespuesta([H|_], IDR, R):- 
getIdRes(H,ID), ID == IDR, R= H.

getRespuesta([_|T], IDR, R):-
getRespuesta(T, IDR, R).


%_________________________________________

%TDA pregunta: representa una preguntas y sus lementos obligatorios.
%Representación: pregunta[entero id pregunta, string nombre del autor, fecha fecha de publicación, string contenido, lista de strings etiquetas,
%string estado pregunta [Abierta/Cerrada], entero número de visualizaciones, entero votos a favor, entero votos en contra, recompensa Recompensa,
% entero reportes, lista de respuestas respuestas].

%Capa constructor:
%Entrada: un entero ID (identificador de pregunta), un string AP (nombre del autor de la pregunta), una fecha FP (fecha de publicación de la pregunta), 
%un string C (contenido pregunta), una lista de strins LE (listas de etiquetas), un string EP (estado de la pregunta), un entero NV (número de visualizaciones),
%un entero VF (votos a favor), un entero VC (votos en contra), una recompensa REC (recompensa con ofertor y monto), una lista de respuestas RESPUESTAS 
%(repuestas de la pregunta), una variable PREGUNTA para asignar la pregunta construida. 
%Salida: una Pregunta (lista de elementos) si las entradas son correctas y un false sino. 

pregunta([]).
crearPregunta(IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS,PREGUNTA):-
integer(IDP),string(AP),esFecha(FP),string(C),esListaString(LE),string(EP),integer(NV),integer(VF),integer(VC),esRecompensa(REC),integer(NR),esListaRespuestas(RESPUESTAS),
IDP > -1, NV > -1, VF > -1, VC > -1, NR > -1,
PREGUNTA= [IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS].

%Ejemplo R1= [0, "Maria", [29, 2, 2020], "¿Por qué es considerado una mala práctica utilizar variables globales?,¿Realmente son perjudiciales?", ["Malas practicas","variables globales"], "Abierta", 10, 3, 2, ["Maria", 10], 1, [[0,"Pedro", [2,3,2020], "Aumenta la complejidad y puede generar resultados impredecibles", ["Variables globales", "Problemas"], "Aceptada",5,2,0]]].

%Capa pertenencia:
%Entrada: una lista de elementos que sirve como postulante a ser una pregunta.
%Salida: un booleano, un true si la lista es una pregunta y un false sino.

esPregunta([]).
esPregunta([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS]):-
integer(IDP),string(AP),esFecha(FP),string(C),esListaString(LE),string(EP),integer(NV),integer(VF),integer(VC),esRecompensa(REC),integer(NR),esListaRespuestas(RESPUESTAS),
IDP > -1, NV > -1, VF > -1, VC > -1, NR > -1 .

%Capa selector:
%Entrada: todos los predicados de la capa selector reciben como entrada una pregunta.

%Salida: un entero, ID de la pregunta.
getIdPreg([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS],IDP):- esPregunta([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS]).

%Salida: un string, nombre del autor de la pregunta.
getAutorPreg([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS],AP):- esPregunta([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS]).

%Salida: una fecha, fecha de publicación de la pregunta.
getIdFechaPreg([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS],FP):- esPregunta([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS]).

%Salida: un string, contenido de la pregunta.
getContenidoPreg([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS],C):- esPregunta([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS]).

%Salida: una lista de strings, lista de etiquetas de la pregunta.
getEtiquetasPreg([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS],LE):- esPregunta([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS]).

%Salida: un string, estado de la pregunta.
getEstadoPreg([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS],EP):- esPregunta([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS]).

%Salida: un entero, número de visualizaciones de la pregunta.
getVisualizaciones([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS],NV):- esPregunta([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS]).

%Salida: un entero, votos a favor de la pregunta.
getVotosAFavorPreg([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS],VF):- esPregunta([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS]).

%Salida: un entero, votos en contra de la pregunta.
getVotosEnContraPreg([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS],VC):- esPregunta([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS]).

%Salida: una recompensa, recompensa ofrecida por la pregunta.
getRecompensaPreg([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS],REC):- esPregunta([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS]).

%Salida: un entero, número de reportes de la pregunta.
getReportesPreg([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS],NR):- esPregunta([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS]).

%Salida: una lista de respuestas, respuestas de la pregunta.
getRespuestasPreg([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS],RESPUESTAS):- esPregunta([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS]).


%_________________________________________

%TDA lista de preguntas: representa una lista de preguntas en el stack.
%Representación: una lista de preguntas [pregunta1, pregunta2, ......., preguntaN].

%Capa pertenencia:
%Entrada: una lista con elementos, sirve como postulante para una lista de preguntas.
%Salida: un booleano, un true si la lista corresponde a una lista de preguntas y un false sino.
esListaPreguntasVacia([]).
esListaPreguntas([]).
esListaPreguntas([H|T]):- esPregunta(H),esListaPreguntas(T).

%Capa constructor:
%Entrada: una lista de respuestas o una nueva lista vacia, la nueva respuesta y una variable para asignar la nueva lista de respuestas con la respuesta incluida.
%Salida: una nueva lista de respuestas.

agregarPregunta([], [IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS], [[IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS]]):-
esPregunta([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS]). 

agregarPregunta([H|T], [IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS],  [H|NEWT]):- 
esListaPreguntas([H|T]),
agregarPregunta(T, [IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS], NEWT). 

%Capa selector: entrega a una pregunta en una lista de preguntas al idendificarla por su id.
%Entrada: una lista de preguntas, un entero IDP (identificador de la pregunta que se desea obtener) y una variable P para asignar la pregunta.
%Salida: un pregunta.

getPregunta([H|_], IDP, P):- 
getIdPreg(H,ID), ID == IDP, P= H.

getPregunta([_|T], IDP, P):-
getPregunta(T, IDP, P).



%Recibe una lista de preguntas.
%agregarResPreg([[IDP,A,FP,C,LE,E,NV,VF,VC,REC,NR,[]]|Preguntas], IDP, NewAnswer, [[IDP,A,FP,C,LE,E,NV,VF,VC,REC,NR,[NewAnswer]]|Preguntas]):-
%esRespuesta(NewAnswer).

%agregarResPreg([[IDP,A,FP,C,LE,E,NV,VF,VC,REC,NR,[Respuesta|Respuestas]]|Preguntas], IDP, NewAnswer, [[IDP,A,FP,C,LE,E,NV,VF,VC,REC,NR,[NewAnswer,Respuesta|Respuestas]]|Preguntas]):-
%esRespuesta(NewAnswer).

%agregarResPreg([Pregunta|Preguntas], IDPreg, NewAnswer, [Pregunta|NewPreguntas]):- agregarResPreg(Preguntas, IDPreg, NewAnswer, NewPreguntas).


 %_________________________________________

%TDA stack: representa a Stack Overflow.
%Representación: stack [usuario Sesión activa, lista de usuarios Registrados, lista de preguntas Preguntas stack, entero Correlativo pregunta, entero Correlativo respuestas].

%Capa constructor:
%Entrada: un usuario UA (usuario activo en el stack), una lista de usuarios LU (usuarios registrados en el stack), una lista de preguntas LP (preguntas del stack),
% un entero CP (correlativo para llevar la cuenta de la cantidad de preguntas en el stack), un entero CR (correlativo para llevar la cuenta de la cantidad de respuestas en el stack).
%Salida: un stack.

%stack([[],[],[],0,0]).
crearStack(UA, LU, LP, CP, CR, STACK):-
esUsuario(UA),esListaUsuarios(LU),esListaPreguntas(LP),integer(CP),integer(CR),
CP > -1 , CR > -1,
STACK= [UA, LU, LP, CP, CR].

%Capa pertenencia:
%Entrada: un lista de elementos que sirven como postulante a ser un stack.
%salida: un boolenao, un true si la lista es un stack y un false sino.

esStack([[],[],[],0,0]).
esStack([UA, LU, LP, CP, CR]):-
esUsuario(UA),esListaUsuarios(LU),esListaPreguntas(LP),integer(CP),integer(CR),
CP > -1 , CR > -1.

%Capa selector:
%Entrada: todos los predicados de la capa selector reciben un stack.

%Salida: un usuario, usuario activo en el stack.
getActivo([UA, LU, LP, CP, CR],UA):- esStack([UA, LU, LP, CP, CR]).

%Salida: una lista de usuarios, registrados en el stack.
getListaUsuarios([UA, LU, LP, CP, CR],LU):- esStack([UA, LU, LP, CP, CR]).

%Salida: una lista de preguntas, preguntas del stack.
getListaPreguntas([UA, LU, LP, CP, CR],LP):- esStack([UA, LU, LP, CP, CR]).

%Salida: un entero, correlativo de preguntas en el stack.
getCorrelativoPreg([UA, LU, LP, CP, CR],CP):- esStack([UA, LU, LP, CP, CR]).

%Salida: un entero, correlativo de respuestas en el stack.
getCorrelativoRes([UA, LU, LP, CP, CR],CR):- esStack([UA, LU, LP, CP, CR]).

%Capa modificador:

actualizarStackRegister([UA, LU, LP, CP, CR], NEWLU, [UA, NEWLU, LP, CP, CR]):- esListaUsuarios(NEWLU).

actualizarStackLogin([UA, LU, LP, CP, CR], REGISTRADO, [REGISTRADO, LU, LP, CP, CR]):- esUsuario(REGISTRADO).

actualizarStackAsk([UA, LU, LP, CP, CR], NEWLP, NEWCOP, [[], LU, NEWLP, NEWCOP, CR]):- esListaPreguntas(NEWLP).


%predicados extras:

agregarUserStack([UA, [[NewNameUser,Pass,Reput,Ref]|Usuarios], LP, CP, CR], NewNameUser, NewPass, [UA, [[NewNameUser,Pass,Reput,Ref]|Usuarios], LP, CP, CR]):- !. %Si existe usuario.
agregarUserStack([UA, [], LP, CP, CR], NameUser, Pass, [UA, [[NameUser, Pass, 0, []]], LP, CP, CR]):- string(NameUser),string(Pass).
agregarUserStack([UA, [Usuario|Usuarios], LP, CP, CR], NewNameUser, NewPass, [UA, [Usuario|NewUsuarios], LP, CP, CR]):-
agregarUserStack([UA, Usuarios, LP, CP, CR], NewNameUser, NewPass, [UA, NewUsuarios, LP, CP, CR]).


autentificarUser([UA, [[NameUser,Pass,Reput,Ref]|Usuarios], LP, CP, CR], NameUser, Pass, [[NameUser,Pass,Reput,Ref], [[NameUser,Pass,Reput,Ref]|Usuarios], LP, CP, CR]):- !. %Si se autentifica usuario.
autentificarUser([UA, [Usuario|Usuarios], LP, CP, CR], NameUser, Pass, [NewUA, [Usuario|NewUsuarios], LP, CP, CR]):-
autentificarUser([UA, Usuarios, LP, CP, CR], NameUser, Pass, [NewUA, NewUsuarios, LP, CP, CR]).


agregarAskStack([[NombreUA,_,_,_], LU, [], CP, CR], FechaP, ContP, Etiq, NewCorrPreg, [[], LU, [[CP, NombreUA, FechaP, ContP, Etiq,"Abierta",0,0,0,["",0],0,[]]], NewCorrPreg, CR]).
agregarAskStack([[NombreUA,_,_,_], LU, [Pregunta|Preguntas], CP, CR], FechaP, ContP, Etiq, NewCorrPreg, [[], LU, [[CP, NombreUA, FechaP, ContP, Etiq,"Abierta",0,0,0,["",0],0,[]], Pregunta|Preguntas], NewCorrPreg, CR]).


%agregarPreguntaStack([UA, LU, [], CP, CR], [NomUser,Pass,Reput,ListRef], [UA, [NomUser,Pass,Reput,ListRef], LP, CP, CR]):- esUsuario([NomUser,Pass,Rep,ListRef]).

%_________________________________________
%Desarrollo requerimiento 2: Hechos.

usuario(["Maria", "Maria1999", 50, ["Racket","c#"]]).
usuario(["Ana","A1234", 70, ["java","python"]]).
usuario(["Juan","juan2000", 20, ["python","c++"]]).
usuario(["Pedro", "P340", 90,  ["python","c++"]]).

pregunta([0, "Maria", [29, 2, 2020], "¿Por que es considerado una mala practica utilizar variables globales?,¿Realmente son perjudiciales?", 
			["Malas practicas","variables globales"], "Abierta", 10, 3, 2, ["Maria", 10], 1, 
 			[[0,"Pedro", [2,3,2020], "Aumenta la complejidad y puede generar resultados impredecibles", ["Variables globales", "Problemas"], "Aceptada",5,2,0],
 			[1, "Juan", [1, 3, 2020], "El problema de las variables globales es que crea dependencias ocultas. Cuando se trata de aplicaciones grandes, ni tu mismo sabes/recuerdas/tienes claro los objetos que tienes y sus relaciones.", ["Malas practicas","errores"], "", 2, 9, 1]
 			]]).

pregunta([1, "Ana", [29, 10, 2020], "¿Como poner una imagen de fondo en? Me gustaria saber ¿Como pongo una imagen de fondo a la ventana creada con PyQT5? Muchos me dicen que use Designer, pero estoy evitando usarlo. ¿Conocen alguna manera?", ["python","interfaz-gráfica","imagen"], "Abierta", 20, 5, 2,["",0], 0,
			[[0,"Pedro", [2,3,2020], "Aumenta la complejidad y puede generar resultados impredecibles", ["Variables globales", "Problemas"], "Aceptada",5,2,0],
			[1, "Juan", [1, 3, 2020], "El problema de las variables globales es que crea dependencias ocultas. Cuando se trata de aplicaciones grandes, ni tu mismo sabes/recuerdas/tienes claro los objetos que tienes y sus relaciones.", ["Malas practicas","errores"], "", 2, 9, 1],
 			[2, "Maria", [13, 11, 2020], "Usando Qt Style Sheet", [], "Rechazada", 6, 3, 0]]]).

preguntas([[1, "Ana", [29, 10, 2020], " ¿Conocen alguna manera de jahsjh?", ["python","interfaz-gráfica"], "Abierta", 20, 5, 2,["",0], 0,
			[[2, "Maria", [13, 11, 2020], "Usando Qt Style Sheet", [], "Rechazada", 6, 3, 0]]],
 			[3, "Paola", [29, 10, 2020], " ¿Saben si jahsj?", ["python","interfaz-gráfica"], "Abierta", 10, 3, 1,["Ana",10], 0, []],
 			[0, "Pedro", [29, 10, 2020], "Esta prueba tambien funciona", ["python","interfaz-gráfica"], "Abierta", 10, 3, 1,["Alma",10], 0, 
 			[[0,"Pam", [2,3,2020], "Si tambien", ["Variables globales", "Problemas"], "Aceptada",6,2,0]]] ],
 			 0, [10,"camila", [2,3,2020], "Si funciona", ["Verificar", "Problemas"], "Aceptada",5,2,0],SF).



stack([["Juan","juan2000", 20, ["python","c++"]],[["Maria", "Maria1999", 50, ["Racket","c#"]],["Ana","A1234", 70, ["java","python"]],["Juan","juan2000", 20, ["python","c++"]],["an","H123",0,[]]], 
			[[0, "Maria", [29, 2, 2020], "¿Por que es considerado una mala practica utilizar variables globales?,¿Realmente son perjudiciales?", 
			["Malas practicas","variables globales"], "Abierta", 10, 3, 2, ["Maria", 10], 1, 
 			[[0,"Pedro", [2,3,2020], "Aumenta la complejidad y puede generar resultados impredecibles", ["Variables globales", "Problemas"], "Aceptada",5,2,0]]],
			[1, "Ana", [29, 10, 2020], "¿Como poner una imagen de fondo en? Me gustaria saber ¿Como pongo una imagen de fondo a la ventana creada con PyQT5? Muchos me dicen que use Designer, pero estoy evitando usarlo. ¿Conocen alguna manera?", ["python","interfaz-gráfica","imagen"], "Abierta", 20, 5, 2,["",0], 0,
			[[0,"Pedro", [2,3,2020], "Aumenta la complejidad y puede generar resultados impredecibles", ["Variables globales", "Problemas"], "Aceptada",5,2,0],
			[1, "Juan", [1, 3, 2020], "El problema de las variables globales es que crea dependencias ocultas. Cuando se trata de aplicaciones grandes, ni tu mismo sabes/recuerdas/tienes claro los objetos que tienes y sus relaciones.", ["Malas practicas","errores"], "", 2, 9, 1],
 			[2, "Maria", [13, 11, 2020], "Usando Qt Style Sheet", [], "Rechazada", 6, 3, 0]]]], 
 			10, 12],[02,10,2010], "esto funciona?",["prueba","ask"],S).

stack([[],[["Maria", "Maria1999", 50, ["Racket","c#"]],["Ana","A1234", 70, ["java","python"]],["Juan","juan2000", 20, ["python","c++"]],["an","H123",0,[]]], 
			[[0, "Maria", [29, 2, 2020], "¿Por que es considerado una mala practica utilizar variables globales?,¿Realmente son perjudiciales?", 
			["Malas practicas","variables globales"], "Abierta", 10, 3, 2, ["Maria", 10], 1, 
 			[[0,"Ana", [2,3,2020], "Aumenta la complejidad y puede generar resultados impredecibles", ["Variables globales", "Problemas"], "Aceptada",5,2,0]]],
			[1, "Ana", [29, 10, 2020], "¿Como poner una imagen de fondo en? Me gustaria saber ¿Como pongo una imagen de fondo a la ventana creada con PyQT5? Muchos me dicen que use Designer, pero estoy evitando usarlo. ¿Conocen alguna manera?", ["python","interfaz-gráfica","imagen"], "Abierta", 20, 5, 2,["",0], 0,
			[[3,"Maria", [2,3,2020], "Aumenta la complejidad y puede generar resultados impredecibles", ["Variables globales", "Problemas"], "Aceptada",5,2,0],
			[1, "Juan", [1, 3, 2020], "El problema de las variables globales es que crea dependencias ocultas. Cuando se trata de aplicaciones grandes, ni tu mismo sabes/recuerdas/tienes claro los objetos que tienes y sus relaciones.", ["Malas practicas","errores"], "", 2, 9, 1],
 			[2, "Maria", [13, 11, 2020], "Usando Qt Style Sheet", [], "Rechazada", 6, 3, 0]]]], 
 			10, 12],"Ana","A1234",S).

answer(S,[2,2,2020],id,"CONTENIDO",["prueba"],S2).





%3Desarrollo predicado register:

register(Stack, NewUserName, PassUser, Stack2):-
esStack(Stack), agregarUserStack(Stack, NewUserName, PassUser, Stack2).


%4Desarroloo predicado login:

login(Stack, UserName, Pass, Stack2):-
esStack(Stack), string(UserName), string(Pass),
autentificarUser(Stack, UserName, Pass, Stack2).


%5Desarrollo predicado ask:

ask([UsuarioActivo, ListUser, ListPreg, CorrPreg, CorrRes], FechaP, ContP, Etiq, Stack2):-
esStack([UsuarioActivo, ListUser, ListPreg, CorrPreg, CorrRes]), esFecha(FechaP), string(ContP), esListaString(Etiq), NewCorrPreg is CorrPreg + 1,
agregarAskStack([UsuarioActivo, ListUser, ListPreg, CorrPreg, CorrRes], FechaP, ContP, Etiq, NewCorrPreg, Stack2).


%PruebaAsk:


([[],[["Maria", "Maria1999", 50, ["Racket","c#"]],["Ana","A1234", 70, ["java","python"]],["Juan","juan2000", 20, ["python","c++"]],["an","H123",0,[]]], 
			[[0, "Maria", [29, 2, 2020], "¿Por que es considerado una mala practica utilizar variables globales?,¿Realmente son perjudiciales?", 
			["Malas practicas","variables globales"], "Abierta", 10, 3, 2, ["Maria", 10], 1, 
 			[[0,"Ana", [2,3,2020], "Aumenta la complejidad y puede generar resultados impredecibles", ["Variables globales", "Problemas"], "Aceptada",5,2,0]]],
			[1, "Ana", [29, 10, 2020], "¿Como poner una imagen de fondo en? Me gustaria saber ¿Como pongo una imagen de fondo a la ventana creada con PyQT5? Muchos me dicen que use Designer, pero estoy evitando usarlo. ¿Conocen alguna manera?", ["python","interfaz-gráfica","imagen"], "Abierta", 20, 5, 2,["",0], 0,
			[[3,"Maria", [2,3,2020], "Aumenta la complejidad y puede generar resultados impredecibles", ["Variables globales", "Problemas"], "Aceptada",5,2,0],
			[1, "Juan", [1, 3, 2020], "El problema de las variables globales es que crea dependencias ocultas. Cuando se trata de aplicaciones grandes, ni tu mismo sabes/recuerdas/tienes claro los objetos que tienes y sus relaciones.", ["Malas practicas","errores"], "", 2, 9, 1],
 			[2, "Maria", [13, 11, 2020], "Usando Qt Style Sheet", [], "Rechazada", 6, 3, 0]]]], 
 			10, 12], "Ana","A1234",SF),answer(SF, [2,2,2020], 1, "Respuesta", ["prueba","funciona"], S2).

%7Desarrollo predicado answer.

	

%Recibe una lista de preguntas.

agregarResPreg([[NUA,CU,RU,REP],LU,[[IDP,A,F,C,LE,E,NV,VF,VC,REC,NR,[]]|Pregs],CP,CR], IDP, Fecha, Cont, Etiq, NCR, [[],LU,[[IDP,A,F,C,LE,E,NV,VF,VC,REC,NR,[[CR,NUA,Fecha,Cont,Etiq,"",0,0,0]]]|Pregs],CP,NCR]):- !.
agregarResPreg([[NUA,CU,RU,REP],LU,[[IDP,A,F,C,LE,E,NV,VF,VC,REC,NR,[Res|Resps]]|Pregs],CP,CR], IDP, Fecha, Cont, Etiq, NCR, [[],LU,[[IDP,A,F,C,LE,E,NV,VF,VC,REC,NR,[[CR,NUA,Fecha,Cont,Etiq,"",0,0,0],Res|Resps]]|Pregs],CP,NCR]):- !.
agregarResPreg([UA,LU,[Preg|Pregs],CP,CR], IDP, Fecha, Cont, Etiq, NCR, [NewUA,LU,[Preg|NewPregs],CP,NCR]):- agregarResPreg([UA,LU,Pregs,CP,CR], IDP, Fecha, Cont, Etiq, NCR, [NewUA,LU,NewPregs,CP,NCR]).


noExisteUsuarioActivo([]):- !.

%predicado para saber si existe una pregunta:
%Utiliza el operador de corte para confirmar la existencia de una pregunta según su id en la lista de preguntas.
existePregEnStack([UA,LU,[[IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS]|_],CP,CR], IDP):- !.
existePregEnStack([UA,LU,[_|Preguntas],CP,CR], IDP):-
existePregEnStack([UA,LU,Preguntas,CP,CR], IDP).


answer([UA,LU,LP,CP,CR], Fecha, IDP, Contenido, ListEtiq, Stack2):-
esStack([UA,LU,LP,CP,CR]), esFecha(Fecha), integer(IDP), string(Contenido), esListaString(ListEtiq), 
existePregEnStack([UA,LU,LP,CP,CR],IDP), not(noExisteUsuarioActivo(UA)), NCR is CR + 1, 
agregarResPreg([UA,LU,LP,CP,CR], IDP, Fecha, Contenido, ListEtiq, NCR, Stack2).




%8Desarrollo predicado accept:


siExisteResAccept([[IDR, AutorRes, FP, C, LE, _, VF, VC, NR]|Respuestas], IDR, AutorRes, [[IDR, AutorRes, FP, C, LE, "Aceptada", VF, VC, NR]|Respuestas]):- !.
siExisteResAccept([Respuesta|Respuestas], IDR, AutorRes, [Respuesta|NewRespuestas]):- siExisteResAccept(Respuestas, IDR, AutorRes, NewRespuestas).

siExistePregDeUserActivoYResAccept([[IDP,AP,FP,C,LE,EP,NV,VF,VC,[_,Monto],NR,Respuestas]|Preguntas], IDP, AP, IDR, AutorRes, Monto, NewPreg):-
siExisteResAccept(Respuestas, IDR, Autor, NRespuestas), AutorRes = Autor, NewPreg = [[IDP,AP,FP,C,LE,EP,NV,VF,VC,["",0],NR,NRespuestas]|Preguntas], !.

siExistePregDeUserActivoYResAccept([Pregunta|Preguntas], IDP, AP, IDR, AutorRes, Monto, [Pregunta|NewPreguntas]):- 
siExistePregDeUserActivoYResAccept(Preguntas, IDP, AP, IDR, AutorRes, Monto, NewPreguntas).

actualizarRepUser([[NameUser,CU,R,LR]|Usuarios], NameUser, Monto, [[NameUser,CU,NewRep,LR]|Usuarios]):- NewRep is R + Monto.
actualizarRepUser([Usuario|Usuarios], NameUser, Monto, [Usuario|NewUsuarios]):- actualizarRepUser(Usuarios, NameUser, Monto, NewUsuarios).


accept([[NomUserActivo,CUA,RUA,REP], ListUser, ListPreg, CorrPreg, CorrRes], IdPreg, IdRes, Stack2):-
esStack([[NomUserActivo,CUA,RUA,REP], ListUser, ListPreg, CorrPreg, CorrRes]), integer(IdPreg), integer(IdRes),
siExistePregDeUserActivoYResAccept(ListPreg, IdPreg, NomUserActivo, IdRes, AutorRes, MontoRecom, NewListPreg), M is MontoRecom + 15,
actualizarRepUser(ListUser, NomUserActivo, 2, ListUser2), actualizarRepUser(ListUser2, AutorRes, M, ListUser3),
Stack2 = [[], ListUser3, NewListPreg, CorrPreg, CorrRes].



%Desarrollo predicado stackToString:

listUserToString([],"\n\n").
listUserToString([[NameUser, Pass, Reputacion, Referencias]|Usuarios], StringFinal):-
atomics_to_string(Referencias,Ref),
atomics_to_string(["Nombre Usuario: ",NameUser, "\nClave: ",Pass, "\nReputacion: ",Reputacion, "\nReferencias: ",Ref, "\n\n"], Str),
listUserToString(Usuarios, StringFinal2), string_concat(Str, StringFinal2, StringFinal).

listResToString([], "\n\n").
listResToString([[IDR, AR, [D,M,A], C, Etiq, EA, VF, VC, NR]|Respuestas], StringRes):-
atomics_to_string(Etiq,E), atomics_to_string([D,"/",M,"/",A],FP),
atomics_to_string(["ID Respuesta: ",IDR, "\nAutor: ",AR, "\nFecha: ",FP , "\nContenido: ",C, "\nEtiquetas: ",E, "\nEstado: ",EA, "\nVotos a favor: ",VF, "\nVotos en contra: ",VC, "\nReportes: ",NR, "\n\n"], Str),
listResToString(Respuestas, StringRes2), string_concat(Str, StringRes2, StringRes).

listPregToString([], "\n").
listPregToString([[IDP,AP,[D,M,A],C,LE,EP,NV,VF,VC,[Ofertor,Monto],NR,Respuestas]|Preguntas],StringPreg):-
atomics_to_string(LE,E), atomics_to_string([D,"/",M,"/",A],FP), atomics_to_string([Ofertor, " ofrece ", Monto, " puntos "],R), listResToString(Respuestas,Res),
atomics_to_string(["ID Pregunta: ",IDP, "\nAutor: ",AP, "\nFecha: ",FP, "\nContenido: ",C, "\nEtiquetas: ",E, "\nEstado: ",EP, "\nVisualizaciones: ",NV,"\nVotos a favor: ",VF, "\nVotos en contra: ",VC,"\nRecompensa: ",R, "\nReportes: ",NR,"\nRespuestas:\n\n",Res,"\n\n"], Str),
listPregToString(Preguntas, StringPreg2), string_concat(Str, StringPreg2, StringPreg).

allPregUsuario([],_,[]).
allPregUsuario([[IDP,NameUser,FP,C,LE,EP,NV,VF,VC,R,NR,Res]|Pregs], NameUser, [[IDP,NameUser,FP,C,LE,EP,NV,VF,VC,R,NR,Res]|NewPreg]):- allPregUsuario(Pregs, NameUser, NewPreg).
allPregUsuario([Preg|Pregs], NameUser, NewPregs):- allPregUsuario(Pregs, NameUser, NewPregs).

stackToString([[], LU, LP, CP, CR], StackStr):- esStack([[], LU, LP, CP, CR]),
listUserToString(LU,LUStr), listPregToString(LP,LPStr), atomics_to_string(["StackOverflow:\n\n", "Usuarios:\n",LUStr, "Preguntas:\n",LPStr], StackStr).

stackToString([[NUA,CU,RU,REP], LU, LP, CP, CR], StackStr):- esStack([[NUA,CU,RU,REP], LU, LP, CP, CR]),
listUserToString([[NUA,CU,RU,REP]],UAStr),allPregUsuario(LP,NUA,NLP), listPregToString(NLP,LPStr),
atomics_to_string(["\nUsuario Activo:\n",UAStr, "Preguntas usuario:\n",LPStr], StackStr).


%Desarrollo predicado vote:


getQuestion([UA,LU,[[IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,Respuestas]|_],CP,CR], IDP, [IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,Respuestas]):- !.
getQuestion([UA,LU,[_|Preguntas],CP,CR], IDP, Pregunta):- getQuestion([UA,LU,Preguntas,CP,CR], IDP, Pregunta).

tomarRespuesta([[IDR,AR,FP,C,LE,EA,VF,VC,NR]|_], IDR, [IDR,AR,FP,C,LE,EA,VF,VC,NR]):- !.
tomarRespuesta([_|Respuestas], IDR, Respuesta):- tomarRespuesta(Respuestas, IDR, Respuesta).

getAnswer([UA,LU,[[IDP,A,F,C,LE,E,NV,VF,VC,REC,NR,Respuestas]|Pregs],CP,CR], IDP, IDR, Respuesta):- tomarRespuesta(Respuestas, IDR, Respuesta).
getAnswer([UA,LU,[_|Preguntas],CP,CR], IDP, IDR, Respuesta):- getAnswer([UA,LU,Preguntas,CP,CR], IDP, IDR, Respuesta).



vote([[NUA,CU,RU,REP],LU,[[IDP,NUA,F,C,LE,E,NV,VF,VC,REC,NR,Resps]|Pregs],CP,CR], [IDP,NUA,F,C,LE,E,NV,VF,VC,REC,NR,Resps], true, [[],NLU,[[IDP,NUA,F,C,LE,E,NV,NVF,NVC,REC,NR,Resps]|Pregs],CP,CR]):- 
NVF is VF + 1, NVC is VC, actualizarRepUser(LU, NUA, 10, NLU).

vote([[NUA,CU,RU,REP],LU,[[IDP,NUA,F,C,LE,E,NV,VF,VC,REC,NR,Resps]|Pregs],CP,CR], [IDP,NUA,F,C,LE,E,NV,VF,VC,REC,NR,Resps], false, [[],NLU,[[IDP,NUA,F,C,LE,E,NV,NVF,NVC,REC,NR,Resps]|Pregs],CP,CR]):- 
NVC is VC + 1, NVF is VF, actualizarRepUser(LU, NUA, -2, NLU).

vote([UA,LU,[Preg|Pregs],CP,CR], Pregunta, Booleano, [NewUA,NewLU,[Preg|NewPregs],CP,CR]):- vote([UA,LU,Pregs,CP,CR], Pregunta, Booleano, [NewUA,NewLU,NewPregs,CP,CR]).






%vote([[NUA,CU,RU,REP], LU, LP, CP, CR], [IDP,NUA,FP,C,LE,EP,NV,VF,VC,REC,NR,Respuestas], true, Stack2):- 






