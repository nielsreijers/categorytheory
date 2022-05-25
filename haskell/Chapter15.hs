phi alpha = alpha id
psi fa h = fmap h fa

zero = psi ([] :: [()])
one  = psi [()]
four = psi [(), (), (), ()]

main = do
    print (phi zero)
    print (phi one)
    print (phi four)
